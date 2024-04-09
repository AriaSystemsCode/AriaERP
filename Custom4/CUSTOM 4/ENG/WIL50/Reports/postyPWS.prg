*:***************************************************************************
*: Program file  : POSTYWS
*: Program desc. : Print Custom PO  for WILSON IMPORTS
*: For Report    : POSTYWS.FRX
*: System        : Aria Advantage Series ARIA4XP  
*: Module        : Purchase Order (PO)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Ticket No     : T20061124.0001
*: Tracking No   : C200740
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSTYPO
*:***************************************************************************
*!*	_SCREEN.Visible = .T.
*!*	ACTIVATE WINDOW TRACE
*!*	SUSPEND


IF llOGFltCh
DIMENSION laSizes(16),laVend [5],lcAd[5]
DIMENSION laprg(12,2)
STORE '' TO lcTmplate1 ,lcTmplate2 ,lcAd

lcMail =ALLTRIM( gfGetMemVar("M_COMMAIL"))


IF USED(lcCompInfo)
  SELECT &lcCompInfo
  * Get the company addresses
  lcAd[1]    = ALLTRIM(caddress1)
  lcAd[2]    = ALLTRIM(caddress2)
  lcAd[3]    = ALLTRIM(caddress3)
  lcAd[5]    = ALLTRIM(caddress5)
 
ENDIF 

=lfBuildTmp()
SELECT (LCPO)
INDEX ON ORDER TAG (LCPO)

SELECT (lcPoLn)
INDEX ON ORDER+CPAGE+ratio+COLOR+SCALE TAG (lcPoLn)

* po -- cutpick -- ordline--ordhdr -- customer
=gfOpenTable('CUTPICK','CUTPKORD','SH','CUTPICK')
=gfOpenTable('ORDLINE','ORDLINE','SH','ORDLINE')
=gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')
=gfOpenTable('PMPRJDT','PMPRJDT','SH','PMPRJDT')
=gfOpenTable('SPCK_LIN','SPCKLINS','SH','SPCK_LIN')
=gfOpenTable('NOTEPAD','NOTEPAD','SH','NOTEPAD')
=gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS')
=gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJLINK')

=lfFillNot()

SELECT CUTPICK
GFSEEK('2')

SELECT POSLN
SET RELATION TO "2"+PO+STR(lineno,6) INTO CUTPICK ADDITIVE 

SELECT CUTPICK 
SET RELATION TO "O"+ORDER+CORDline INTO ORDLINE ADDITIVE 

SELECT ORDLINE
SET RELATION TO "O"+ORDER INTO ORDHDR ADDITIVE 
SET RELATION TO IIF(EMPTY(ORDLINE.Store),'M'+ORDLINE.Account,'S'+ORDLINE.account + ORDLINE.Store) INTO CUSTOMER ADDITIVE 

SELECT POSHDR
SET SKIP to
SET RELATION OFF INTO "posln" 

SELECT posln
SET RELATION OFF INTO "SCALE" 
SET RELATION OFF INTO "OBJLINK" 

SELECT POSHDR

SCAN FOR &lcRpExp 
  =SEEK(POSHDR.CBUSDOCU+POSHDR.CSTYTYPE+POSHDR.PO,'posln')
  STORE '' TO laVend ,lcVendName   ,lcVenFax     
  =lfFillVend()
  m.CvenName=lcVendName   
  m.CvenAdr1=laVend[1]
  m.CvenAdr2=laVend[2]
  m.CvenAdr3=laVend[3]
  m.CvenAdr4=laVend[4]
  m.CvenAdr5=laVend[5]
	M.ORDER= PO
	M.STYMAJOR = LEFT(Posln.style,lnMajSize)
	m.DESC1=STYLE.DESC1
	M.ENTERED=POSHDR.ENTERED
	M.FABWGHT = Style.CFABWGHT
	M.BRAND = style.cbranding
	M.UNT_CST = POSLN.nFcost1
	M.CURR =IIF(SEEK(Poshdr.cpricecur,'SycCurr'),SycCurr.ccurrdesc,'')
	lnTOT_QTY = 0
	lnUnt_Crt = 0
	M.KIMBL=IIF(POSHDR.CKIMBAL,'Yes','No')
	M.DESIGN=gfCodDes(STYLE.CWDESIGNER, 'CWDESIGNER')
	M.RNGNAME=STYLE.CRANGE
	M.CUSTPO=ORDHDR.CUSTPO
	M.ACCOUNT=ORDLINE.ACCOUNT
	M.CUSTOMER=CUSTOMER.BTNAME
	M.SHPTRMS=gfCodDes(poshdr.cshipterm, 'CSHIPTERM')
	M.PROMOTION=gfCodDes(ORDHDR.SPCINST, 'SPCINST')
	M.PACK=gfCodDes(POSHDR.CPACK, 'CPACK')
	M.QC=gfCodDes(POSHDR.CQC, 'CQC')
	M.HANDLE=gfCodDes(CUSTOMER.CACHANDL, 'CACHANDL')
	M.NWRPT=gfCodDes(POSHDR.CORDSEQ, 'CORDSEQ')
	M.POINS1=lcTmplate1 
	M.POINS2=lcTmplate2
	M.CPCKINS=ALLTRIM(POSHDR.CPCKINS1)+', '+ALLTRIM(POSHDR.CPCKINS2)
	M.CPCKINS=IIF(ISNULL(M.CPCKINS),' ',M.CPCKINS)
	M.CBCINS=ALLTRIM(POSHDR.CBCINS1)+', '+ALLTRIM(POSHDR.CBCINS2)+', '+ALLTRIM(POSHDR.CBCINS3)+', '+ALLTRIM(POSHDR.CBCINS4)
	M.CBCINS=IIF(ISNULL(M.CBCINS),' ',M.CBCINS)
	M.GRTHAN9=lfGetExt(style.scale)
	m.RATXT='Ratio'
	M.SHPDATE=POSHDR.CSHIPDATE
	= lfGetPrj(ordline.order)
	FOR lnI=1 TO 16
    lcI=ALLTRIM(STR(lnI))
    m.Sz&lcI=laSizes(lnI)
    IF lnI<13
	    m.prj_Desc&lcI=laprg(lnI,1)
	    m.prj_Date&lcI=IIF(!EMPTY(laprg(lnI,2)),laprg(lnI,2),{})
    ENDIF 
  ENDFOR 
  INSERT INTO (LCPO) FROM MEMVAR
  SELECT POSLN
  SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO=POSHDR.CBUSDOCU+POSHDR.CSTYTYPE+POSHDR.PO 
  	lnsz=lfGetSzArr(POSLN.SCALE)
    *convert 14,  3 to variable
	  M.COLOR=SUBSTR(ALLTRIM(gfCodDes(SUBSTR(POSLN.STYLE,14,3), 'COLOR')),1,12)+IIF(!EMPTY(SCALE.CDIM1),"_"+ALLTRIM(SCALE.CDIM1),'')
      =lfInsBar(POSLN.STYLE,lnsz,M.COLOR)    
	  lnLn=IIF(lnsz=1,0,8)
		lnUnt_Crt =IIF(lnUnt_Crt >0,  lnUnt_Crt  ,IIF(POSLN.CPACKTYPE='C',POSLN.NINPACKQTY,0))
		
	IF SEEK (M.ORDER+'11'+M.COLOR,lcPoln)
    FOR lnI=1 TO 8
      lnT=lnI+lnLn
      lcT=ALLTRIM(STR(lnT))
      lcI=ALLTRIM(STR(lnI))
      REPLACE  &lcPoln..qty&lcT WITH &lcPoln..qty&lcT+IIF(!EOF('cutpick'),cutpick.qty&lcI,posln.qty&lcI),;
                &lcPoln..TOTQTY WITH &lcPoln..TOTQTY+IIF(!EOF('cutpick'),cutpick.qty&lcI,posln.qty&lcI)
      lnTOT_QTY =lnTOT_QTY +IIF(!EOF('cutpick'),cutpick.qty&lcI,posln.qty&lcI)        
    ENDFOR 
    IF !EMPTY(ORDLINE.PREPAK) AND SEEK('P'+POSLN.SCALE+ORDLINE.PRePaK,'SCALE') AND SEEK (M.ORDER+'12'+M.COLOR,lcPoln)
	    FOR lnI=1 TO 8
	      lnT=lnI+lnLn
	      lcT=ALLTRIM(STR(lnT))
        lcI=ALLTRIM(STR(lnI))
        REPLACE  &lcPoln..pp&lcT WITH SCALE.PP&lcI ,&lcPoln..TOTPP WITH &lcPoln..TOTPP+SCALE.PP&lcI
        
	    ENDFOR   
    ENDIF 
  ELSE
    FOR lnI=1 TO 16
      lcI=ALLTRIM(STR(lnI))
	    m.qty&lcI=0
	    m.PP&lcI=0
    ENDFOR 
    M.TOTQTY=0
    M.NOTES=SUBSTR(ORDLINE.NOTE_MEM,1,124)
    FOR lnI=1 TO 8
      lnT=lnI+lnLn
      lcT=ALLTRIM(STR(lnT))
      lcI=ALLTRIM(STR(lnI))
      m.qty&lcT=IIF(!EOF('cutpick'),cutpick.qty&lcI,posln.qty&lcI)
      M.TOTQTY=m.TOTQTY+m.qty&lcT
      m.ratio='1'
      M.CPAGE='1' 
    ENDFOR   
     lnTOT_QTY =lnTOT_QTY +M.TOTQTY
     M.TOTPP= 0
    INSERT INTO (LCPOln) FROM MEMVAR
    IF !EMPTY(ORDLINE.PREPAK) AND SEEK('P'+POSLN.SCALE+ORDLINE.PRePaK,'SCALE')
	    FOR lnI=1 TO 8
	      lnT=lnI+lnLn
	      lcT=ALLTRIM(STR(lnT))
        lcI=ALLTRIM(STR(lnI))
        m.pp&lcT=SCALE.PP&lcI 
        M.TOTPP= M.TOTPP+m.pp&lcT
	      m.qty&lcT=0
	      m.ratio='2' 
	      M.CPAGE='1'            
	    ENDFOR  
	     M.TOTQTY=0 
	    INSERT INTO (LCPOln) FROM MEMVAR
    ENDIF 
  ENDIF
  
	ENDSCAN	
	IF !SEEK (M.ORDER+'12',lcPoln)
       SELECT (lcPoln)
       APPEND BLANK
       REPLACE  &lcPoln..ORDER WITH M.ORDER ,&lcPoln..CPAGE WITH '1',&lcPoln..RATIO WITH '2'
       SELECT (lcPo)
    ENDIF    
	
  SELECT (LCPO)
	REPLACE   tot_qty WITH lnTOT_QTY ,TOT_CST WITH lnTOT_QTY*UNT_CST,;
	          UNT_CRT WITH IIF(lnUnt_Crt >0,ALLTRIM(STR(lnUnt_Crt )),'TBC'),;
	          TOT_CRT WITH IIF(lnUnt_Crt >0,ALLTRIM(STR(CEILING(lnTOT_QTY/lnUnt_Crt ))),'TBC')
	
*!*		IF SEEK ('A'+M.ACCOUNT , 'OBJLINK') AND SEEK(OBJLINK.COBJECT_ID ,'OBJECTS')
*!*			SELECT (LCPOln) 
*!*			APPEND BLANK
*!*			REPLACE ORDER   WITH POSHDR.PO ,CPAGE WITH '3',RATIO WITH '4'
*!*		ENDIF
    SELECT (LCPOln) 
	APPEND BLANK
	REPLACE ORDER   WITH POSHDR.PO ,CPAGE WITH '3',RATIO WITH '4'

	IF !SEEK (M.ORDER+'23',lcPoln)
	  SELECT (LCPOln) 
	  APPEND BLANK
	  REPLACE ORDER   WITH poshdr.PO ,CPAGE WITH '2',RATIO WITH '3'
	ENDIF 
	
ENDSCAN

SELECT (LCPOln) 
REPLACE ALL QQ9_16 WITH QTY9+QTY10+QTY11+QTY12+QTY13+QTY14+QTY15+QTY16 ,;
            PP9_16 WITH PP9+PP10+PP11+PP12+PP13+PP14+PP15+PP16 
SELECT POSLN
SET RELATION TO 

SELECT CUTPICK 
SET RELATION TO 

SELECT ORDLINE
SET RELATION TO 

SELECT OBJLINK 
SET RELATION TO

SELECT (LCPO)
SET RELATION TO 'A'+ACCOUNT INTO OBJLINK 

SELECT OBJLINK 
SET RELATION TO COBJECT_ID INTO OBJECTS 

SELECT (LCPOLN)
SET RELATION TO ORDER INTO (LCPO) ADDITIVE 
STORE ' .T. ' TO lcRpExp
ENDIF
SELECT (LCPOLN)
*!*************************************************************
*! Name      : lfGetExt
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 01/28/2007
*! Purpose   : Get ext. Scales
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGetExt
LPARAMETERS lcscale
lnalias =SELECT()
STORE '' TO laSizes
STORE 'N' TO GRT9
STORE '' TO lcline1,lcline2
SELECT SCALE
STORE 0 TO lnCnt
SEEK('S'+ALLTRIM(LEFT(lcscale,2)))
SCAN REST WHILE type+scale='S'+ALLTRIM(LEFT(lcscale,2))
  lnCnt=lnCnt+1
  IF lnCnt=3
    EXIT
  ENDIF
IF lnCnt=1
  FOR lnI=1 TO 8
    lcI=ALLTRIM(STR(lnI))
    laSizes(lnI)=scale.sz&lcI
    lcline1=lcline1+ALLTRIM(scale.sz&lcI)
  ENDFOR   
ELSE
 	  FOR lnI=1 TO 8
 	    lcI=ALLTRIM(STR(lnI))
    laSizes(lnI+8)=scale.sz&lcI
    lcline2=lcline2+ALLTRIM(scale.sz&lcI)
  ENDFOR  
  IF lcline1<>lcline2
   	  GRT9='Y'
  ENDIF  	  
  
ENDIF 
ENDSCAN

SELECT (lnAlias)
RETURN GRT9


*!*************************************************************
*! Name      : lfGetSzArr
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 01/28/2007
*! Purpose   : Get ext. Scales
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGetSzArr
LPARAMETERS lcscale
lnalias =SELECT()
SELECT SCALE
LNCNT=0
SEEK('S'+LEFT(ALLTRIM(lcscale),2))
SCAN REST WHILE type+scale='S'+LEFT(ALLTRIM(lcscale),2)
LNCNT=LNCNT+1
  IF SCALE.SCALE=lcscale OR LNCNT=2
    EXIT
  ENDIF
ENDSCAN

RETURN LNCNT
*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[76,18]
STORE '' TO laTempStru
STORE 0 TO lnIndex

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STYMAJOR'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = lnMajSize
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'DESC1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 40
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ENTERED'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'FABWGHT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0
*5
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BRAND'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'UNT_CST'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 3

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CURR'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TOT_QTY'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 3

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TOT_CST'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 3
*10
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'UNT_CRT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'KIMBL' && YES\NO
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 3
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'DESIGN'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'RNGNAME'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SHPTRMS'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0
*15
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CUSTOMER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PROMOTION'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PACK'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'QC'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 60
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'HANDLE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*20
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'NWRPT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

FOR lnI=1 TO 16
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'SZ'+ALLTRIM(STR(lnI))
	laTempStru[lnIndex,2] = 'C'
	laTempStru[lnIndex,3] = 6
	laTempStru[lnIndex,4] = 0
ENDFOR 

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'GRTHAN9'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CPCKINS'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 125
laTempStru[lnIndex,4] = 0

FOR lnI=1 TO 12
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'prj_desc'+ALLTRIM(STR(lnI))
	laTempStru[lnIndex,2] = 'C'
	laTempStru[lnIndex,3] = 50
	laTempStru[lnIndex,4] = 0
	
	lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'prj_date'+ALLTRIM(STR(lnI))
	laTempStru[lnIndex,2] = 'D'
	laTempStru[lnIndex,3] = 10
	laTempStru[lnIndex,4] = 0
ENDFOR 
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CUSTPO'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CBCINS'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 240
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'POINS1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 240
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'POINS2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 240
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ACCOUNT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SHPDATE'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TOT_CRT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CvenName'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CvenAdr1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CvenAdr2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CvenAdr3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CvenAdr4'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CvenAdr5'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

=gfCrtTmp(lcPo,@laTempstru,,"",.f.)

DIMENSION laTempStru[44,18]
STORE '' TO laTempStru
STORE 0 TO lnIndex
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'COLOR'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 18
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'RATIO'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

FOR lnI=1 TO 16
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'QTY'+ALLTRIM(STR(lnI))
	laTempStru[lnIndex,2] = 'N'
	laTempStru[lnIndex,3] = 10
	laTempStru[lnIndex,4] = 2
ENDFOR 

FOR lnI=1 TO 16
  lnIndex=lnIndex+1
  laTempStru[lnIndex,1] = 'PP'+ALLTRIM(STR(lnI))
	laTempStru[lnIndex,2] = 'N'
	laTempStru[lnIndex,3] = 10
	laTempStru[lnIndex,4] = 2
ENDFOR 
*35
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TOTQTY'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CPAGE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SCALE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BARCODE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 40
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'RATXT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'NOTES'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 124
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TOTPP'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'QQ9_16'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PP9_16'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

=gfCrtTmp(lcPoln,@laTempstru,,"",.f.)


*!*************************************************************
*! Name      : lfGetPrj
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 01/28/2007
*! Purpose   : Get Project Tasks
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGetPrj
LPARAMETERS lcOrder
lnalias =SELECT()
STORE '' TO laprg
lnI=0
SELECT PMPRJDT
=SEEK('O'+ALLTRIM(lcOrder))
SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id='O'+ALLTRIM(lcOrder) FOR !LSHW2CUST
  lnI=lnI+1
  laprg(lnI,1)=PMPRJDT.coprt_dsc
  laprg(lnI,2)=PMPRJDT.dclc_fnsh

  IF lnI=12
    EXIT 
  ENDIF 
ENDSCAN
SELECT (lnAlias)
RETURN ""



*!*************************************************************
*! Name      : lfInsBar
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 01/28/2007
*! Purpose   : Get Barcode for styles
*!*************************************************************
*! Parameters: =lfInsBar(POSLN.STYLE,lnsz)    
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfInsBar
LPARAMETERS lcStyle,lnScale,lcColor

lnalias =SELECT()
SELECT SPCK_LIN
=SEEK('S'+ORDLINE.ACCOUNT+ALLTRIM(lcStyle))
SCAN REST WHILE type+account+style+pack_id='S'+ORDLINE.ACCOUNT+ALLTRIM(lcStyle)
  DO CASE
    CASE QTY1=1
      lcSz=IIF(lnScale=1,lasizes(1),lasizes(9))
    CASE QTY2=1
      lcSz=IIF(lnScale=1,lasizes(2),lasizes(10))
    CASE QTY3=1
      lcSz=IIF(lnScale=1,lasizes(3),lasizes(11))
    CASE QTY4=1
      lcSz=IIF(lnScale=1,lasizes(4),lasizes(12))
    CASE QTY5=1
      lcSz=IIF(lnScale=1,lasizes(5),lasizes(13))
    CASE QTY6=1
      lcSz=IIF(lnScale=1,lasizes(6),lasizes(14))
    CASE QTY7=1
      lcSz=IIF(lnScale=1,lasizes(7),lasizes(15))
    CASE QTY8=1
      lcSz=IIF(lnScale=1,lasizes(8),lasizes(16))
  ENDCASE 
  IF !SEEK (M.ORDER+'23'+M.COLOR+ALLTRIM(lcSz),lcPoln)
	  SELECT (LCPOln) 
	  APPEND BLANK
	  REPLACE ORDER   WITH POSLN.PO ,CPAGE WITH '2',RATIO WITH '3',COLOR WITH lcColor,;
	          BARCODE WITH SPCK_LIN.PACK_ID,SCALE WITH ALLTRIM(lcSz)
  ENDIF 
ENDSCAN

SELECT (lnAlias)
RETURN ""
*!*************************************************************
*! Name      : lfFillNot
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 01/28/2007
*! Purpose   : Fill Notepad Varables fot PO templates
*!*************************************************************
*! Parameters: ..
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfFillNot

lnalias =SELECT()
SELECT NOTEPAD
*!*	IF SEEK('TPOINS1')
IF SEEK('TPOINS')

  lcTmplate1 =NOTEPAD.MNOTES
ENDIF

IF SEEK('TPOINS2')
  lcTmplate2 =NOTEPAD.MNOTES
ENDIF

SELECT (lnAlias)
RETURN ""

*!************************************************************
*! Name      : lfFillVend
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 01/28/2007
*! Purpose   : Fill Vendore Data
*!*************************************************************
*! Parameters: ..
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfFillVend

IF POSHDR.CBUSDOCU+POSHDR.cStyType # 'NN'
  lcVendName   = APVENDOR.CVenComp
  lcVenFax     = APVENDOR.CFAXNO
  * Get the vendor addresses
  laVend[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVend[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVend[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVend[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVend[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ELSE
    =SEEK(LEFT(PosHdr.Vendor,6),'WAREHOUS')
    lcVendName   = WAREHOUS.cDesc
    * Get warehouse fax number
    lcVenFax     = WAREHOUS.cFAX
    laVend[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    laVend[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    laVend[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    laVend[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
    laVend[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF

lnALen = ALEN(laVend,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laVend[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laVend[lnC]=laVend[lnC+1]
    ENDFOR
    laVend[lnAlen]=''
  ENDIF
ENDFOR
