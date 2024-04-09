*!*:**********************************************************************
*: Program file       : ICPLIST
*: Program description: Price List Report
*: Module             : Inventory Control (IC)
*: Developer          : Tarek Noaman (TNA)
*: Tracking Job Number: N037566
*: Date               : 11/14/2005
*:**********************************************************************
*: Calls: 
*:         Programs   : 
*:         Screens    : 
*: Global Function    :gfItemMask,gfDispRe,gfRltFld
*:**********************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters  : 
*:***************************************************************************
*: Example : DO ICPLSIT
*:***************************************************************************
*: MODIFICATION:
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004]
*:***************************************************************************
*:                   Option grid Filter contains 
*:1-Price Level                    ALL/A/B/C/Retail            lcPrice   
*:2-Print  price after disc.       YES/NO                      llrpPdic   
*:3-Style - Color                  Between                     llRpOrdNot
*:***************************************************************************
*:                         Tables Used
*:                        _______________
*:01- Style
*:02- Scale
*:***************************************************************************

*!*	_SCREEN.Visible = .T.
*!*	ACTIVATE WINDOW TRACE
*!*	SUSPEND


lcStTime   = TIME()
lcPrice = IIF(EMPTY(lcPrice),'L',lcPrice)
lnDisc  = 0
DIMENSION laDisc[1,2]
laDisc[1,1] = 'DISCPCNT'
laDisc[1,2] = "lnDisc"

PRIVATE lcSqlTempStatmentS,lcStyleFields,lcTempStyle
STORE .F. TO llSqlErro,lcTempStyle,llStyle

IF loOgScroll.llOGFltCh 

  lcStyleFlds    ="style.style,style.desc1,style.desc,style.cstygroup,style.cstymajor,style.scale,style.cdisccode,style.pricea,style.priceb,style.pricec,style.nsugretpri"
  lcStyleFlds    =lcStyleFlds    +[,"   " AS CCURRCODE ]
  lcStyleFields  ="style.style,style.desc1,style.[desc],style.cstygroup,style.cstymajor,style.scale,style.cdisccode,style.pricea,style.priceb,style.pricec,style.nsugretpri"
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN
  lcStyleFields  =lcStyleFields  +" ,SPACE(3) AS CCURRCODE "
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..END

  lcSqlTempStatmentSty= "SELECT "+lcStyleFields+" FROM Style "  

  SELECT &lcStyleFlds FROM Style WHERE .F. INTO CURSOR TStyle READWRITE

  lnBuffering = CURSORGETPROP("Buffering",'TStyle')
   =CURSORSETPROP("Buffering",3,'TStyle')
  *-- To initialize the indecis that will be created for each file
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN
*!*	  INDEX ON Cstymajor TAG 'TStyle' &&OF (lcCursor)
  INDEX ON STYLE+CCURRCODE TAG 'TStyle' &&OF (lcCursor)
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN

  SET ORDER TO TAG 'TStyle' 
 
 
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,'STYLE.STYLE')
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    lcTempStyle = loOgScroll.laOGFxFlt[lnPOS,6]
  ENDIF
  llStyle = !EMPTY(lcTempStyle) AND USED(lcTempStyle) AND RECCOUNT(lcTempStyle) > 0
	*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN
	 IF llMultCurr 
	  lnCUR = ASCAN(loOgScroll.laOGFxFlt,'STYPRICE.CCURRCODE')
	  IF lnCUR > 0
	    lnCUR = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnCUR ,1)
	    lcTempCurr = loOgScroll.laOGFxFlt[lnCUR ,6]
	  ENDIF
	  LLCURS = !EMPTY(lcTempCurr ) AND USED(lcTempCurr ) AND RECCOUNT(lcTempCurr ) > 0
	 lcCurExp=IIF(LLCURS,"SEEK(ccurrcode ,lcTempCurr )",".T.")
 ENDIF 
	*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN

  DO CASE 
    CASE llStyle
*      lcSqlTempStatmentSty = lcSqlTempStatmentSty + " WHERE Style.Style='"
      SELECT (lcTempStyle)
      SCAN 
        lcTempStyleNo=style
        loDBFStyle.Seek(lcTempStyleNo,'Style')
        SELECT Style
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN
*!*	        SCAN  
*!*	          SCATTER MEMVAR memo
*!*	          INSERT INTO TStyle FROM MEMVAR 
*!*	        ENDSCAN 
           SCATTER MEMVAR memo
          IF !llMultCurr OR !LLCURS OR GFSEEK(gcBaseCurr,lcTempCurr )
            M.CCURRCODE=gcBaseCurr
            INSERT INTO TStyle FROM MEMVAR 
          ENDIF
          IF llMultCurr   AND GFSEEK(lcTempStyleNo,'STYPRICE')
          SELECT STYPRICE
            SCAN  REST  WHILE   style +ccurrcode =lcTempStyleNo FOR &lcCurExp
              m.ccurrcode=STYPRICE.ccurrcode
              m.pricea=STYPRICE.pricea
              m.priceb=STYPRICE.priceb
              m.pricec=STYPRICE.pricec
              m.nsugretpri=STYPRICE.nsugretpri
              INSERT INTO TStyle FROM MEMVAR 
            ENDSCAN 
          ENDIF

*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT  ..END

      ENDSCAN
    OTHERWISE 
*:T20061124.0004 12/19/2006 AYM : eNHANCEMENT FOR PRICE LIS REPORT ..BEGIN
*!*	      SELECT Style
*!*	      SCAN 
*!*	        SCATTER MEMVAR memo
*!*	        INSERT INTO TStyle FROM MEMVAR 
*!*	      ENDSCAN 
      
     SELECT Style
     SCAN
       SCATTER MEMVAR memo
        SCATTER MEMVAR memo
        IF !llMultCurr OR !LLCURS OR GFSEEK(gcBaseCurr,lcTempCurr )
          M.CCURRCODE=gcBaseCurr
          INSERT INTO TStyle FROM MEMVAR 
        ENDIF
       IF llMultCurr   AND GFSEEK(STYLE,'STYPRICE')
          SELECT STYPRICE
          SCAN  REST  WHILE   style +ccurrcode =STYLE.STYLE FOR &lcCurExp
            m.ccurrcode=STYPRICE.ccurrcode
            m.pricea=STYPRICE.pricea
            m.priceb=STYPRICE.priceb
            m.pricec=STYPRICE.pricec
            m.nsugretpri=STYPRICE.nsugretpri
            INSERT INTO TStyle FROM MEMVAR 
          ENDSCAN 
        ENDIF
     ENDSCAN  
     
      
      
      
      
      
  ENDCASE
  
ENDIF 

SELECT TStyle
*!*	SET ORDER TO tag 'TStyle' 
lcEdTime = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--

WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Style(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcRpName')


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Tarek Noaman
*! Date      : 06/10/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lcStyTtl  = gfItemMask('HI')
lcStyPict = '@! '+gfItemMask('PI')
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
*loDBFStyle  = CreateObject("RemoteTable","Style",,"Style",SET("DATASESSION"),,.T.) 
*loDBFScale  = CreateObject("RemoteTable","Scale",,"Scale",SET("DATASESSION"),,.T.) 
loDBFStyle  = CreateObject("RemoteTable","Style","Style","Style",SET("DATASESSION"),,.T.) 
loDBFScale  = CreateObject("RemoteTable","Scale","Scale","Scale",SET("DATASESSION"),,.T.) 
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][End]
RETURN


*!*************************************************************
*! Name      : lfGetScl
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to get style scale.
*!*************************************************************
FUNCTION lfGetScl
PARAMETERS lcScale

lcSclDesc =''

IF loDBFScale.SEEK('S'+lcScale,'SCALE')
  FOR I=1 TO SCALE.Cnt
    lcZ = STR(I,1)
    lcSclDesc = lcSclDesc + ALLTRIM(SCALE.SZ&lcZ)+IIF(I=SCALE.Cnt,'',' - ')
  ENDFOR
ENDIF

IF EMPTY(TStyle.cDiscCode)
  lnDisc = 0
ELSE
  =gfRltFld(TStyle.cDiscCode,@laDisc,'CDISCCODE')
ENDIF

RETURN lcSclDesc


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.


