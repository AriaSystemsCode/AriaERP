*:**************************************************************************
*: Program file  : MABARC
*: Program desc. : Bar Code Printing
*: System        : Aria Advantage Series.A4XP
*: Module        : Materials (MA)
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 02/15/2007
*: Reference     : N000585 - T20061227.0005     
*:**************************************************************************
*: Calls : 
*:    Procedures : lpCollData, lpCreatFil
*:                   
*:    Functions  : gfModalGen, lfwRepWhen, lfItmPos, lfSRRoll, lfSRVFab, 
*:                 lfFabSum, lfClearRep
*:**************************************************************************
*: Notes : This program works from Output and also from Transaction menu
*:         so we have variable called lcCallFrom defined in Transaction Prg
*:         MAINVC before calling the Report 
*:**************************************************************************
*: Example : DO MABARC
*:**************************************************************************
*: Modifications:
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*:**************************************************************************
*-- If user changed filter from OG [Begin.]
IF loogScroll.llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF
  =lfCollect()  
ENDIF 
loogScroll.cCROrientation = 'P'  
SELECT (lcWorkFile)

IF RECCOUNT(lcWorkFile) <> 0
  DO gfDispRe WITH (lcRpName),'',.F.,'L'
ELSE     
  *-- <Message> No records to display.
  *-- <Buttons>          OK
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : OG when function
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

=gfOpenTable('ITEM','STYLE','SH')
=gfOpenTable('ROLLS','ROLLS','SH')
=gfOpenTable('POSLN','POSLN','SH')
=gfOpenTable('POSHDR','POSHDR','SH')
=gfOpenTable('UOM','uomcode','SH')


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
	
	lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)&&PADR(Rolls.crollitem ,lnMajLength )+lcSepar+PADR(Rolls.color,lenClrlen)
	
	SELECT Rolls
	DIMENSION laRollStruct[1,18]
	=AFIELDS(laRollStruct)
	
	*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
	*= gfCrtTmp(lcTmpRolls,@laRollStruct,'cRollId + cRollItem + Color + Dyelot + cWareCode',lcTmpRolls,.F.)
	= gfCrtTmp(lcTmpRolls,@laRollStruct,'cRollId + Style + Dyelot + cWareCode',lcTmpRolls,.F.)
	*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
	
	SELECT Rolls
	=gfSeek('')     
	
	*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*		SCAN FOR Rolls.TranCD = '1' AND IIF(TYPE('lcCallFrom') = "C" AND TYPE('llFromMan') = "L" ,IIF(llFromMan,;
*!*		    gfSeek('0002'+PADR(Rolls.crollitem ,lnMajLength )+lcSepar+PADR(Rolls.color,lenClrlen),'ITEM') AND ITEM.MAKE,gfSeek('0002'+PADR(Rolls.crollitem ,lnMajLength )+lcSepar+PADR(Rolls.color,lenClrlen),'ITEM') AND !ITEM.MAKE),.T.)
*!*		  IF !SEEK(Rolls.cRollId + Rolls.cRollItem + Rolls.Color + Rolls.Dyelot + Rolls.cWareCode ,lcTmpRolls,lcTmpRolls) 
	SCAN FOR Rolls.TranCD = '1' AND IIF(TYPE('lcCallFrom') = "C" AND TYPE('llFromMan') = "L" ,IIF(llFromMan,;
	    gfSeek('0002'+Rolls.sTYLE,'ITEM') AND ITEM.MAKE,gfSeek('0002'+ROLLS.STYLE,'ITEM') AND !ITEM.MAKE),.T.)
	  IF !SEEK(Rolls.cRollId + Rolls.sTYLE + Rolls.Dyelot + Rolls.cWareCode ,lcTmpRolls,lcTmpRolls) 
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]	  
	    SCATTER MEMO MEMVAR 
	    SELECT (lcTmpRolls)
	    APPEND BLANK 
	    GATHER MEMO MEMVAR 
	  ENDIF 
	  
	ENDSCAN 
ENDIF 

*-- Get the Position of the Rolls ID position in OG
lnRollPost = lfItmPos('ROLLS.CROLLID')

*-- Get the Position of the Item position in OG
lnItemPost = lfItmPos('ROLLS.CROLLITEM')

*-- Get the Position of the Fabric Color position in OG
lnColorPos = lfItmPos('ROLLS.COLOR')

DO lpCreatFil   && Create the Work Temp file
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loogscroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : Create work File.
*!**************************************************************************
*! Called from : Report code.
*!**************************************************************************
*! Example   : DO lpCreatFil
*!**************************************************************************
*
PROCEDURE lpCreatFil

*nYarDage  --> Rolls.nQty
*cUOMBuy   --> Fabric.UOMBuy
*cBarCode  --> Rolls.cRollId

DIMENSION laStructArr [13,4]

laStructArr[1,1] = 'cFabric'
laStructArr[1,2] = 'C'

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*laStructArr[1,3] = 7
laStructArr[1,3] = 19
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

laStructArr[1,4] = ''

laStructArr[2,1] = 'cFabDesc'
laStructArr[2,2] = 'C'
laStructArr[2,3] = 20
laStructArr[2,4] = ''

laStructArr[3,1] = 'cColor'
laStructArr[3,2] = 'C'
laStructArr[3,3] = 6
laStructArr[3,4] = ''

laStructArr[4,1] = 'cDyelot'
laStructArr[4,2] = 'C'
laStructArr[4,3] = 10
laStructArr[4,4] = ''

laStructArr[5,1] = 'nYarDage'
laStructArr[5,2] = 'N'
laStructArr[5,3] = 12
laStructArr[5,4] = 3

laStructArr[6,1] = 'cPattern'
laStructArr[6,2] = 'C'
laStructArr[6,3] = 10
laStructArr[6,4] = ''

laStructArr[7,1] = 'cUOMBuy'
laStructArr[7,2] = 'C'
laStructArr[7,3] = 3
laStructArr[7,4] = ''

laStructArr[8,1] = 'cVendor'
laStructArr[8,2] = 'C'
laStructArr[8,3] = 8
laStructArr[8,4] = ''

laStructArr[9,1] = 'dTranDate'
laStructArr[9,2] = 'D'
laStructArr[9,3] = 8
laStructArr[9,4] = ''

laStructArr[10,1] = 'cLocation'
laStructArr[10,2] = 'C'
laStructArr[10,3] = 10
laStructArr[10,4] = ''


laStructArr[11,1] = 'cFabWidth'
laStructArr[11,2] = 'C'
laStructArr[11,3] = 6
laStructArr[11,4] = ''

laStructArr[12,1] = 'cBarCode'
laStructArr[12,2] = 'C'
laStructArr[12,3] = 20
laStructArr[12,4] = ''

laStructArr[13,1] = 'cUomUse'
laStructArr[13,2] = 'C'
laStructArr[13,3] = 3
laStructArr[13,4] = ''

= gfCrtTmp(lcWorkFile,@laStructArr,'cBarCode+cFabric',lcWorkFile,.T.)

*-- End of lpCreatFil.
*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
*-- Close temp. opended files, if it is used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF

*-- Erase the SQL file
IF USED(lcTmpRolls)
  USE IN (lcTmpRolls)
ENDIF
ERASE (gcWorkDir+lcTmpRolls+'.DBF')
ERASE (gcWorkDir+lcTmpRolls+'.CDX')
*-- End of lfClearRep.


*!**************************************************************************
*! Name      : lfvPrnBy
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : "Print by"  valid function
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfvPrnBy()
*!**************************************************************************
FUNCTION lfvPrnBy
CLEARREAD()
*!*************************************************************
*! Name      : lfsumfab1
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
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
*!*************************************************************
*! Name      : lfConvertToCursor
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
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


*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*: Date      : 02/18/2007
*! Purpose   : Collect data
*!**************************************************************************
FUNCTION lfCollect
*Roll ID
lcRollFile  = ''
llUseRollID   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ROLLS.CROLLID'),1)
IF lnPosition > 0
  lcRollFile  = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUseRollID = IIF(!EMPTY(lcRollFile) .AND. USED(lcRollFile) .AND. RECCOUNT(lcRollFile)>0,.T.,.F.)
ENDIF
IF llUseRollID 
  SELECT(lcRollFile)
  LOCATE 
  IF EOF()
    llUseRollID = .F.
  ENDIF 
ENDIF 

*Item
lcItemFile  = ''
llUseItem   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ROLLS.CROLLITEM'),1)
IF lnPosition > 0
  lcItemFile = LOOGSCROLL.laOGFXFlt[lnPosition,6]
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
lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ROLLS.COLOR'),1)
IF lnClrPos > 0
  lcClrStr = LOOGSCROLL.laOGFXFlt[lnClrPos,6]
  lcClrFile = loOGScroll.gfTempName()
  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
ENDIF

*****************
lcPODate1 = '.T.'
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSLN.DATE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
  lnSepPos = AT('|',LOOGSCROLL.laOGFxFlt[lnPosition,6])
  IF !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1))) .OR.;
     !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))
     lcPODate1 = 'BETWEEN(POSLN.DATE,CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1)),CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))'
  ENDIF
ENDIF

lcPODate2 = '.T.'
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'ALLTRIM(POSLN.DATE)'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
  lnSepPos = AT('|',LOOGSCROLL.laOGFxFlt[lnPosition,6])
  IF !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1))) .OR.;
     !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))
     lcPODate2  = 'BETWEEN(POSLN.DATE,CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1)),CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))'
  ENDIF
ENDIF

*PO1
lcPoFile1  = ''
llUsePO1   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'POSLN.PO'),1)
IF lnPosition > 0
  lcPoFile1 = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUsePO1  = IIF(!EMPTY(lcPoFile1) .AND. USED(lcPoFile1) .AND. RECCOUNT(lcPoFile1)>0,.T.,.F.)
ENDIF
IF llUsePO1   
  SELECT(lcPoFile1)
  LOCATE 
  IF EOF()
    llUsePO1 = .F.
  ENDIF 
ENDIF 

*PO2
lcPoFile2  = ''
llUsePO2   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ALLTRIM(POSLN.PO)'),1)
IF lnPosition > 0
  lcPoFile2 = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUsePO2 = IIF(!EMPTY(lcPoFile2) .AND. USED(lcPoFile2) .AND. RECCOUNT(lcPoFile2)>0,.T.,.F.)
ENDIF
IF llUsePO2   
  SELECT(lcPoFile2)
  LOCATE 
  IF EOF()
    llUsePO2 = .F.
  ENDIF 
ENDIF 
lcSepar   = SUBSTR(lcStyPic,lnMajLength+1,1)&&PADR(Rolls.crollitem ,lnMajLength )+lcSepar+PADR(Rolls.color,lenClrlen)
IF TYPE('lcCallFrom') = "C"
  PRIVATE lcRollsID
  lcRollsID = ''
  IF llUseRollID
    SELECT Rolls
    =gfSetOrder('ROLLS')
    SELECT (lcRollFile)
    SCAN
      IF gfSeek(&lcRollFile..crollid,'Rolls')
        SELECT Rolls
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [START]
*!*	        SCAN REST WHILE CROLLID+CROLLITEM+COLOR+TRANCD =&lcRollFile..crollid FOR ;
*!*	             IIF(llUseItem,SEEK(Rolls.crollitem,lcItemFile),.T.) AND ;
*!*	             IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.);
*!*	             AND TranCd = '1' AND nQtyBal > 0
*!*	          IF !(lcRollsID == crollid+sTYLE)
*!*	            IF gfSEEK('0002'+Rolls.sTYLE,'ITEM','Style')

        SCAN REST WHILE CROLLID+sTYLE+TRANCD =&lcRollFile..crollid FOR ;
             IIF(llUseItem,SEEK(PADR(SUBSTR(Rolls.STyle,1,lnMajLength),19),lcItemFile),.T.) AND ;
             IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.);
             AND TranCd = '1' AND nQtyBal > 0
          IF !(lcRollsID == crollid+sTYLE)
            IF gfSEEK('0002'+Rolls.sTYLE,'ITEM','Style')
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [ENd]
               m.cFabDesc  = ITEM.Desc  
               m.cFabWidth = ITEM.CITEMFLD1
               m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_b,"")
               m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUY,'UOM') OR gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_V,"")
               m.cPattern  = ITEM.Pattern
            ENDIF 
	        m.cLocation = cWareCode
	        
	        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *m.cFabric   = cRollItem
            m.cFabric   = SUBSTR(Rolls.STyle,1,lnMajLength)
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
            
     	    m.cColor    = Color
	        m.cDyelot   = Dyelot	
            m.nYarDage  = nQtyBal
            m.cBarCode  = cRollId
  		    INSERT INTO (lcWorkFile) FROM MEMVAR 
          ENDIF 
        ENDSCAN   
      ENDIF 
    ENDSCAN 
  ELSE
    IF llUseItem
      SELECT Rolls
      =gfSetOrder('ROLLITEM')
      SELECT (lcItemFile)
      SCAN
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        *IF gfSeek(PADR(&lcItemFile..cstymajor,7),'Rolls')
        IF gfSeek(&lcItemFile..cstymajor,'Rolls')
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        
          SELECT Rolls
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	          SCAN REST WHILE CROLLITEM+COLOR+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION =PADR(&lcItemFile..cstymajor,7);
*!*	            FOR IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.);
*!*	             AND TranCd = '1' AND nQtyBal > 0
*!*	            IF !(lcRollsID == crollid+crollitem+color)
*!*	              IF gfSEEK('0002'+PADR(Rolls.crollitem ,lnMajLength )+lcSepar+PADR(Rolls.color,lenClrlen),'ITEM','Style')
          SCAN REST WHILE sTYLE+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION =&lcItemFile..cstymajor;
            FOR IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.);
             AND TranCd = '1' AND nQtyBal > 0
            IF !(lcRollsID == crollid+sTYLE)
              IF gfSEEK('0002'+Rolls.sTYLE,'ITEM','Style')
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                 m.cFabDesc  = ITEM.Desc  
                 m.cFabWidth = ITEM.CITEMFLD1
                 m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_b,"")
                 m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUY,'UOM') OR gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_V,"")
                 m.cPattern  = ITEM.Pattern
              ENDIF 
	          m.cLocation = cWareCode
	          
	          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
              *m.cFabric   = cRollItem
              m.cFabric   = SUBSTR(Rolls.STyle,1,lnMajLength)
              *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
              
     	      m.cColor    = Color
              m.cDyelot   = Dyelot	
              m.nYarDage  = nQtyBal
              m.cBarCode  = cRollId
  	  	      INSERT INTO (lcWorkFile) FROM MEMVAR 
            ENDIF 
          ENDSCAN    
        ENDIF 
      ENDSCAN 
    ELSE
     SELECT Rolls
     gfSeek('')
     SCAN  FOR IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.);
         AND TranCd = '1' AND nQtyBal > 0
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	        IF !(lcRollsID == crollid+crollitem+color)
*!*	          IF gfSEEK('0002'+PADR(Rolls.crollitem ,lnMajLength )+lcSepar+PADR(Rolls.color,lenClrlen),'ITEM','Style')
        IF !(lcRollsID == crollid+sTYLE)
          IF gfSEEK('0002'+Rolls.sTYLE,'ITEM','Style')
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End] 
             m.cFabDesc  = ITEM.Desc  
             m.cFabWidth = ITEM.CITEMFLD1
             m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_b,"")
             m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUY,'UOM') OR gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_V,"")
             m.cPattern  = ITEM.Pattern
          ENDIF 
          m.cLocation = cWareCode

		  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *m.cFabric   = cRollItem
          m.cFabric   = SUBSTR(Rolls.STyle,1,lnMajLength)
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          
 	      m.cColor    = Color
          m.cDyelot   = Dyelot	
          m.nYarDage  = nQtyBal
          m.cBarCode  = cRollId
 	      INSERT INTO (lcWorkFile) FROM MEMVAR 
        ENDIF 
      ENDSCAN    
    ENDIF    
  ENDIF 
ELSE
  SELECT Rolls
  =gfSetOrder('SESSION')
  IF lcRpPrntBy = 'P'
    IF llUsePO1 
      SELECT(lcPoFile1)
      SCAN 
        IF gfSeek('PM'+&lcPoFile1..PO+'0002','POSLN','POSLN')
          SELECT POSLN
          SCAN REST WHILE ;
            cbusdocu+ cstytype+ po+ cinvtype+ style+ STR(lineno,6)+ trancd ='PM'+&lcPoFile1..PO+'0002' ;
            FOR EVALUATE(lcPODate1) AND POSLN.TRANCD = "2"
            
              *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
              *lcKey = cRSession+PADR(SUBSTR(Style,1,lnMajLength),7)+RIGHT(Style,lenClrlen)+cWareCode+Dyelot
              lcKey = cRSession+Style+cWareCode+Dyelot
              *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
              
              IF gfSEEK(lcKey,'Rolls')
                SELECT Rolls
                
                *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                *SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey ;
                  FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                    IIF(llUseItem,SEEK(Rolls.crollitem,lcItemFile),.T.) AND ;
                    IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
                 SCAN REST WHILE csession+sTYLE+cwarecode+dyelot = lcKey ;
                  FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                    IIF(llUseItem,SEEK(PADR(SUBSTR(Rolls.STyle,1,lnMajLength),19),lcItemFile),.T.) AND ;
                    IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)                    
			   *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                    
                  IF gfSEEK('0002'+PoSln.Style,'ITEM','Style') 
                    m.cFabDesc  = ITEM.Desc  
                    m.cFabWidth = ITEM.CITEMFLD1
                    m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_b,"")
                    m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUY,'UOM') OR gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_V,"")                  
                  ENDIF 
                  m.cLocation = cWareCode
                  
                  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                  *m.cFabric   = PADR(SUBSTR(posln.Style,1,lnMajLength),7)                  
                  m.cFabric   = SUBSTR(posln.Style,1,lnMajLength)
                  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                  
                  m.cColor    = RIGHT(posln.Style,lenClrlen)
			  	  m.cDyelot   = PoSLn.Dyelot	
		          m.cPattern  = PoSLn.Pattern
		          m.dTranDate = PoSLn.Date
		          m.cVendor   = PoSLn.Vendor
		          m.nYarDage  = nQtyBal
		          m.cBarCode   = cRollId
		          INSERT INTO (lcWorkFile) FROM MEMVAR 
                 ENDSCAN           
              ENDIF 
    	  ENDSCAN 	
        ENDIF 
      ENDSCAN 
    ELSE
      IF gfSeek('PM','POSLN','POSLN')
        SELECT POSLN
        SCAN REST WHILE ;
          cbusdocu+ cstytype+ po+ cinvtype+ style+ STR(lineno,6)+ trancd ='PM' ;
          FOR EVALUATE(lcPODate1) AND Cinvtype ='0002' AND POSLN.TRANCD = "2" 
	        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *lcKey = cRSession+PADR(SUBSTR(Style,1,lnMajLength),7)+RIGHT(Style,lenClrlen)+cWareCode+Dyelot
            lcKey = cRSession+Style+cWareCode+Dyelot
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
            
            IF gfSEEK(lcKey,'Rolls')
              SELECT Rolls
	        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]              
              *SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey ;
                FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                  IIF(llUseItem,SEEK(Rolls.crollitem,lcItemFile),.T.) AND ;
                  IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
              SCAN REST WHILE csession+STYLE+cwarecode+dyelot = lcKey ;
                FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                  IIF(llUseItem,SEEK(PADR(SUBSTR(Rolls.STyle,1,lnMajLength),19),lcItemFile),.T.) AND ;
                  IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
	        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]                  
                  
                IF gfSEEK('0002'+PoSln.Style,'ITEM','Style') 
                  m.cFabDesc  = ITEM.Desc  
                  m.cFabWidth = ITEM.CITEMFLD1
                  m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_b,"")
                  m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUY,'UOM') OR gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_V,"")                  
                ENDIF 
                m.cLocation = cWareCode
                
                *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                *m.cFabric   = PADR(SUBSTR(posln.Style,1,lnMajLength),7)
                m.cFabric   = SUBSTR(posln.Style,1,lnMajLength)
                *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                
                m.cColor    = RIGHT(posln.Style,lenClrlen)
      		  	  m.cDyelot   = PoSLn.Dyelot	
	              m.cPattern  = PoSLn.Pattern
    	          m.dTranDate = PoSLn.Date
    	          m.cVendor   = PoSLn.Vendor
    	          m.nYarDage  = nQtyBal
    	          m.cBarCode   = cRollId
    	          INSERT INTO (lcWorkFile) FROM MEMVAR 
             ENDSCAN           
           ENDIF 
   	    ENDSCAN 	
      ENDIF 
    ENDIF 
  ELSE
    IF lcRpPrntBy = 'M'
    IF llUsePO2 
      SELECT(lcPoFile2)
      SCAN 
        IF gfSeek('PF'+&lcPoFile2..PO+'0002','POSLN','POSLN')
          SELECT POSLN
          SCAN REST WHILE ;
            cbusdocu+ cstytype+ po+ cinvtype+ style+ STR(lineno,6)+ trancd ='PM'+&lcPoFile2..PO+'0002' ;
            FOR EVALUATE(lcPODate2) AND POSLN.TRANCD = "2"
    	      
    	      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]	
              *lcKey = cRSession+PADR(SUBSTR(Style,1,lnMajLength),7)+RIGHT(Style,lenClrlen)+cWareCode+Dyelot
              lcKey = cRSession+Style+cWareCode+Dyelot
     	      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
     	      
              IF gfSEEK(lcKey,'Rolls')
                SELECT Rolls
                
    	      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]	                
                *SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey ;
                  FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                    IIF(llUseItem,SEEK(Rolls.crollitem,lcItemFile),.T.) AND ;
                    IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
                 SCAN REST WHILE csession+STYLE+cwarecode+dyelot = lcKey ;
                  FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                    IIF(llUseItem,SEEK(PADR(SUBSTR(Rolls.STyle,1,lnMajLength),19),lcItemFile),.T.) AND ;
                    IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
    	      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]	
    	         
                  IF gfSEEK('0002'+PoSln.Style,'ITEM','Style') 
                    m.cFabDesc  = ITEM.Desc  
                    m.cFabWidth = ITEM.CITEMFLD1
                    m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_b,"")
                    m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUY,'UOM') OR gfSeek(ITEM.CCONVBUY,'UOM'),UOM.Cuom_V,"")                  
                  ENDIF 
                  m.cLocation = cWareCode
                 
                 *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                 * m.cFabric   = PADR(SUBSTR(posln.Style,1,lnMajLength),7)                  
                  m.cFabric   = SUBSTR(posln.Style,1,lnMajLength)
                  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                  
                  m.cColor    = RIGHT(posln.Style,lenClrlen)
                  m.cDyelot   = PoSLn.Dyelot  
                  m.cPattern  = PoSLn.Pattern
                  m.dTranDate = PoSLn.Date
                  m.cVendor   = PoSLn.Vendor
                  m.nYarDage  = nQtyBal
                  m.cBarCode   = cRollId
                  INSERT INTO (lcWorkFile) FROM MEMVAR 
                ENDSCAN           
              ENDIF 
            ENDSCAN   
          ENDIF 
        ENDSCAN 
      ELSE
        IF gfSeek('PF','POSLN','POSLN')
          SELECT POSLN
          SCAN REST WHILE ;
            cbusdocu+ cstytype+ po+ cinvtype+ style+ STR(lineno,6)+ trancd ='PF' ;
            FOR EVALUATE(lcPODate1) AND Cinvtype ='0002' AND POSLN.TRANCD = "2" 
            
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *lcKey = cRSession+PADR(SUBSTR(Style,1,lnMajLength),7)+RIGHT(Style,lenClrlen)+cWareCode+Dyelot
            lcKey = cRSession+Style+cWareCode+Dyelot
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                        
            IF gfSEEK(lcKey,'Rolls')
              SELECT Rolls
              
              *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
              *SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey ;
                FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                  IIF(llUseItem,SEEK(Rolls.crollitem,lcItemFile),.T.) AND ;
                  IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
               SCAN REST WHILE csession+sTYLE+cwarecode+dyelot = lcKey ;
                FOR IIF(llUseRollID,SEEK(rolls.crollid,lcRollFile),.T.) AND ;
                  IIF(llUseItem,SEEK(PADR(SUBSTR(Rolls.STyle,1,lnMajLength),19),lcItemFile),.T.) AND ;
                  IIF(llUseClr ,SEEK(rolls.color,lcClrFile),.T.)
   			*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
   
                IF gfSEEK('0002'+PoSln.Style,'ITEM','Style') 
                  m.cFabDesc  = ITEM.Desc  
                  m.cFabWidth = ITEM.CITEMFLD1
                  m.cUOMBuy   = IIF(gfSeek(ITEM.CCONVBUy,'UOM'),UOM.Cuom_b,"")
                  m.cUOMUse   = IIF(SEEK(ITEM.CCONVBUy,'UOM') OR gfSeek(ITEM.CCONVBUy,'UOM'),UOM.Cuom_V,"")                  
                ENDIF 
                m.cLocation = cWareCode
                
                *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                *m.cFabric   = PADR(SUBSTR(posln.Style,1,lnMajLength),7)
                m.cFabric   = SUBSTR(posln.Style,1,lnMajLength)
                *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                
                m.cColor    = RIGHT(posln.Style,lenClrlen)
                m.cDyelot   = PoSLn.Dyelot  
                m.cPattern  = PoSLn.Pattern
                m.dTranDate = PoSLn.Date
                m.cVendor   = PoSLn.Vendor
                m.nYarDage  = nQtyBal
                m.cBarCode   = cRollId
                INSERT INTO (lcWorkFile) FROM MEMVAR 
             ENDSCAN           
           ENDIF 
         ENDSCAN   
      ENDIF 
    ENDIF 
   ENDIF 
  ENDIF 
ENDIF 