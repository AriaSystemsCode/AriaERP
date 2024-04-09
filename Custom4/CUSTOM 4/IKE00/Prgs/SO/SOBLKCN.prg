**********************************************************************************
*! Program   : SOBLKCN.PRG
*! Developer : Sara Osama
*! Date      : 10/27/2016
*! Purpose   : Bulks Cancellation Report screen for IKE00
*! Entry#    : C201886 [T20160920.0014]
****************************************************************************
DO FORM (oAriaApplication.ClientScreenHome+"SO\SOBLKCN.scx")
*!**************************************************************************
*! Name      : lfProceed
*! Developer : Sara Osama
*! Date      : 10/27/2016
*! Purpose   : Processing 
*!**************************************************************************
FUNCTION lfProceed
PARAMETERS loFormSet
ldFrom=loFormSet.AriaForm1.dtpFrom.text1.Value
ldTo=loFormSet.AriaForm1.dtpTo.text1.Value
*Validation*
IF EMPTY(ldTo)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'To date is required, cannot proceed.')
  RETURN .F.
ENDIF
IF !EMPTY(ldFrom) AND ldFrom > ldTo
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'From date is greater than To date. Cannot proceed.')
  RETURN .F.
ENDIF
lcCursorTemp = gfTempName()
llNonBulkFound = .F.
*Opening tables*
IF !USED("ORDLINE")
  =gfOpenTable("ORDLINE","ORDLINE")
ENDIF

IF !USED("ORDLINE_B")
  =gfOpenTable("ORDLINE","ORDBLKST",'SH',"ORDLINE_B")
ENDIF

IF !USED("ORDHDR")
  =gfOpenTable("ORDHDR","ORDHDR")
ENDIF

IF !USED("ORDHDR_B")
  =gfOpenTable("ORDHDR","ORDBULK",'SH',"ORDHDR_B")
ENDIF

IF !USED("Scale")
  =gfOpenTable("Scale","Scale")
ENDIF

IF !USED("warehous")
  =gfOpenTable("warehous","warehous")
ENDIF

IF !USED("customer")
  =gfOpenTable("customer","customer")
ENDIF
lnClrStart = 0
LNCOLORLEN = 0
IF !USED("style")
  =gfOpenTable("style","style")
ENDIF

lfCreatTempTable()
lfFillTempData()
************************** Export Excel file ****************************
SELECT (lcCursorTemp)
SET ORDER TO
COUNT TO lnCount FOR !DELETED()
IF lnCount > 0 
  lcFile = GETFILE('XLS','Browse Location','Ok',0,'Browse Location')
  IF !EMPTY(ALLTRIM(lcFile))
    EXPORT TO (lcFile) TYPE XLS
    IF FILE(lcFile)
      loRun = CreateObject("WScript.Shell")
      loRun.Run(lcFile, 3)
      loRun = NULL
    ENDIF
  ENDIF 
ELSE 
  IF llNonBulkFound 
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'There is no bulk order to display. Only non bulk orders exist in the choosen period.')
  ELSE
    SELECT ORDHDR
    =gfSeek('O')
    LOCATE REST WHILE cOrdType+Order = 'O' FOR STATUS $ 'OH' AND BULK ='Y' AND ORDHDR.entered <= ldto
    IF FOUND()
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is no non-bulk order to display. Only bulk orders exist in the choosen period.')
    ELSE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is no records to display allocated in the choosen period.')
    ENDIF 
  ENDIF
ENDIF 
*!**************************************************************************
*! Name      : lfCreatTempTable
*! Developer : Sara Osama
*! Date      : 10/27/2016
*! Purpose   : Creat Temperary Table 
*!**************************************************************************
FUNCTION lfCreatTempTable

  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT ORDLINE
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 4, 18]

  laTempStru[ALEN(laTempStru,1) -3  ,1] = 'ccontref'
  laTempStru[ALEN(laTempStru,1) -3  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -3  ,3] = 30
  laTempStru[ALEN(laTempStru,1) -3  ,4] = 0
  laTempStru[ALEN(laTempStru,1) -3  ,17] = 0
  laTempStru[ALEN(laTempStru,1) -3  ,18] = 0

  *-- cCurrCode :  used if multi currency only to sort by it.
  laTempStru[ALEN(laTempStru,1) -2 ,1] = 'cCurrCode'
  laTempStru[ALEN(laTempStru,1) -2 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -2 ,3] = 3
  laTempStru[ALEN(laTempStru,1) -2 ,4] = 0
  laTempStru[ALEN(laTempStru,1) -2 ,17] = 0
  laTempStru[ALEN(laTempStru,1) -2 ,18] = 0

  *-- cCurrCode :  used if multi currency only to sort by it.
  laTempStru[ALEN(laTempStru,1) -1 ,1] = 'Dist_ctr'
  laTempStru[ALEN(laTempStru,1) -1 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -1 ,3] = 8
  laTempStru[ALEN(laTempStru,1) -1 ,4] = 0
  laTempStru[ALEN(laTempStru,1) -1  ,17] = 0
  laTempStru[ALEN(laTempStru,1) -1  ,18] = 0

  *-- cCurrCode :  used if multi currency only to sort by it.
  laTempStru[ALEN(laTempStru,1)  ,1] = 'CType'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 1
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  laTempStru[ALEN(laTempStru,1)  ,17] = 0
  laTempStru[ALEN(laTempStru,1)  ,18] = 0

  DIMENSION laTempStru[ALEN(laTempStru,1) + 16, 18]

  laTempStru[ALEN(laTempStru,1)-10  ,1] = 'CDIVISION'
  laTempStru[ALEN(laTempStru,1)-10  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-10  ,3] = 6
  laTempStru[ALEN(laTempStru,1)-10  ,4] = 0
  laTempStru[ALEN(laTempStru,1)-10  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-10  ,18] = 0

  laTempStru[ALEN(laTempStru,1)-15  ,1] = 'DESC'
  laTempStru[ALEN(laTempStru,1)-15  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-15  ,3] = 20
  laTempStru[ALEN(laTempStru,1)-15  ,4] = 0
  laTempStru[ALEN(laTempStru,1)-15  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-15  ,18] = 0

  laTempStru[ALEN(laTempStru,1)-14  ,1] = 'REP1'
  laTempStru[ALEN(laTempStru,1)-14  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-14  ,3] = 3
  laTempStru[ALEN(laTempStru,1)-14  ,4] = 0
  laTempStru[ALEN(laTempStru,1)-14  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-14  ,18] = 0

  laTempStru[ALEN(laTempStru,1)-13  ,1] = 'REP2'
  laTempStru[ALEN(laTempStru,1)-13  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-13  ,3] = 3
  laTempStru[ALEN(laTempStru,1)-13  ,4] = 0
  laTempStru[ALEN(laTempStru,1)-13  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-13  ,18] = 0

  laTempStru[ALEN(laTempStru,1)-12  ,1] = 'Status'
  laTempStru[ALEN(laTempStru,1)-12  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-12  ,3] = 1
  laTempStru[ALEN(laTempStru,1)-12  ,4] = 0
  laTempStru[ALEN(laTempStru,1)-12  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-12  ,18] = 0

  laTempStru[ALEN(laTempStru,1)-11  ,1] = 'priority'
  laTempStru[ALEN(laTempStru,1)-11  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-11  ,3] = 3
  laTempStru[ALEN(laTempStru,1)-11  ,4] = 0
  laTempStru[ALEN(laTempStru,1)-11  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-11  ,18] = 0


  laTempStru[ALEN(laTempStru,1)-9  ,1] = 'cdesc'
  laTempStru[ALEN(laTempStru,1)-9 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-9  ,3] = 35
  laTempStru[ALEN(laTempStru,1)-9 ,4] = 0
  laTempStru[ALEN(laTempStru,1)-9 ,17] = 0
  laTempStru[ALEN(laTempStru,1)-9,18] = 0

  laTempStru[ALEN(laTempStru,1)-8 ,1] = 'btname'
  laTempStru[ALEN(laTempStru,1)-8  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)-8  ,3] = 30
  laTempStru[ALEN(laTempStru,1)-8 ,4] = 0
  laTempStru[ALEN(laTempStru,1)-8  ,17] = 0
  laTempStru[ALEN(laTempStru,1)-8 ,18] = 0
  lnCntDWn = 7
  FOR lnM = 1 TO 8
    lcM = STR(lnM,1)
    laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,1] = 'SZ'+lcM
    laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,2] = 'C'
    laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,3] = 5
    laTempStru[ALEN(laTempStru,1)-lnCntDWn  ,4] = 0
    laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,17] = 0
    laTempStru[ALEN(laTempStru,1)-lnCntDWn  ,18] = 0
    lnCntDWn = lnCntDWn -  1
  ENDFOR

  LOCAL lnColumns
  lnColumns = ALEN(laTempStru,1) + 6
  DIMENSION laTempStru[lnColumns, 18]
  laTempStru[lnColumns - 1 ,1] = 'StyGrpDsc'
  laTempStru[lnColumns - 1 ,2] = 'C'
  laTempStru[lnColumns - 1 ,3] = 30
  laTempStru[lnColumns - 1 ,4] = 0
  laTempStru[lnColumns - 1 ,17] = 0
  laTempStru[lnColumns - 1 ,18] = 0

  laTempStru[lnColumns ,1] = 'isBulk'
  laTempStru[lnColumns ,2] = 'C'
  laTempStru[lnColumns ,3] = 3
  laTempStru[lnColumns ,4] = 0
  laTempStru[lnColumns ,17] = 0
  laTempStru[lnColumns ,18] = 0
  laTempStru[lnColumns - 2 ,1] = 'approval'
  laTempStru[lnColumns - 2 ,2] = 'C'
  laTempStru[lnColumns - 2 ,3] = 10
  laTempStru[lnColumns - 2 ,4] = 0
  laTempStru[lnColumns - 2 ,17] = 0
  laTempStru[lnColumns - 2 ,18] = 0
  laTempStru[lnColumns - 3 ,1] = 'Note2'
  laTempStru[lnColumns - 3 ,2] = 'C'
  laTempStru[lnColumns - 3 ,3] = 30
  laTempStru[lnColumns - 3 ,4] = 0
  laTempStru[lnColumns - 3 ,17] = 0
  laTempStru[lnColumns - 3 ,18] = 0
  
  laTempStru[lnColumns - 4 ,1] = 'Note1'
  laTempStru[lnColumns - 4 ,2] = 'C'
  laTempStru[lnColumns - 4 ,3] = 30
  laTempStru[lnColumns - 4 ,4] = 0
  laTempStru[lnColumns - 4 ,17] = 0
  laTempStru[lnColumns - 4 ,18] = 0

  laTempStru[lnColumns - 5 ,1] = 'ClrCodDsc'
  laTempStru[lnColumns - 5 ,2] = 'C'
  laTempStru[lnColumns - 5 ,3] = 30
  laTempStru[lnColumns - 5 ,4] = 0
  laTempStru[lnColumns - 5 ,17] = 0
  laTempStru[lnColumns - 5 ,18] = 0

LOCAL lnField, lnCol
FOR lnField = 1 TO ALEN(laTempStru,1)
  FOR lnCol = 7 TO 16
    laTempStru[lnField, lnCol] = ""
  ENDFOR
  STORE 0 TO laTempStru[lnField, 17],laTempStru[lnField, 18]
ENDFOR
*-- Create temporary file that holding order line data.
= gfCrtTmp(lcCursorTemp ,@laTempStru,"Order+STYLE" ,lcCursorTemp ,.T.)

************************************************************
*! Name      : lfGetAccStyBulk
*! Developer : Sara Osama
*! Date      : 10/25/2016
*! Purpose   : Check for Bulk order lines
************************************************************
FUNCTION lfGetAccStyBulk
LPARAMETERS lcAccount,lcStyleMaj
llHasOrdLine = .F.
SELECT ORDHDR_B
m.isBulk = 'Yes'
*=gfSeek('O') gfSeek(lcAccount+'H'+'Y'+'O','ORDHDR_B') 
=gfSeek(lcAccount+'O'+'Y'+'O','ORDHDR_B') 
SCAN REST WHILE  account+status+bulk+cordtype+order = lcAccount+'O'+'Y'+'O' FOR ORDHDR_B.entered <= ldto
  IF gfseek('O'+ORDHDR_B.order+lcStyleMaj,'ORDLINE_B')
    SELECT ORDLINE_B
    = gfSeek('O'+ORDHDR_B.Order)
    SCAN REST WHILE cordtype+order+style+pack_id+cpkcolor+cpcksize+cpkversion+store ='O'+ORDHDR_B.order+lcStyleMaj FOR TotQty > 0
      SCATTER MEMO MEMVAR 
      =SEEK(m.Style,'Style','Style')
      m.ClrCodDsc = gfCodDes(SUBSTR(Style.Style ,lnClrStart, lnColorLen),"COLOR")
      m.StyGrpDsc = gfCodDes(Style.cStyGroup ,'CSTYGROUP')
      m.approval = ORDHDR_B.approval
      m.ccontref = ORDHDR_B.ccontref 
      m.Note1 = ORDHDR_B.Note1
      m.Note2 = ORDHDR_B.Note2
	  m.cCurrCode = ORDHDR_B.cCurrCode
	  =SEEK('S'+m.Account+m.Store,'CUSTOMER')
      m.Dist_ctr = Customer.Dist_ctr
      m.Status = ORDHDR_B.Status
      m.priority= ORDHDR_B.Priority
      =SEEK('S' + Scale,'SCALE')
      =SEEK(cWareCode,'WAREHOUS')
      m.cdesc = Warehous.cDesc
      =SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account,'S' + m.Account + m.Store),'Customer')
      m.btname = Customer.BtName
      m.SZ1  = Scale.Sz1
      m.SZ2  = Scale.Sz2
      m.SZ3  = Scale.Sz3
      m.SZ4  = Scale.Sz4
      m.SZ5  = Scale.Sz5
      m.SZ6  = Scale.Sz6
      m.SZ7  = Scale.Sz7
      m.SZ8  = Scale.Sz8
      m.CDIVISION = ORDHDR_B.CDIVISION
      m.REP1  = ORDHDR_B.REP1
      m.REP2  = ORDHDR_B.REP2
      m.DESC  = Style.Desc
      INSERT INTO (lcCursorTemp) FROM MEMVAR
      llHasOrdLine = .T.
    ENDSCAN 
  ENDIF  
ENDSCAN 
SELECT ORDHDR_B
=gfSeek(lcAccount+'H'+'Y'+'O','ORDHDR_B') 
SCAN REST WHILE account+status+bulk+cordtype+order = lcAccount+'H'+'Y'+'O' FOR ORDHDR_B.entered <= ldto
  IF gfseek('O'+ORDHDR_B.order+lcStyleMaj,'ORDLINE_B')
    SELECT ORDLINE_B
    = gfSeek('O'+ORDHDR_B.Order)
    SCAN REST WHILE cordtype+order+style+pack_id+cpkcolor+cpcksize+cpkversion+store ='O'+ORDHDR_B.order+lcStyleMaj FOR TotQty > 0
      SCATTER MEMO MEMVAR 
      =SEEK(m.Style,'Style','Style')
      m.ClrCodDsc = gfCodDes(SUBSTR(Style.Style ,lnClrStart, lnColorLen),"COLOR")
      m.StyGrpDsc = gfCodDes(Style.cStyGroup ,'CSTYGROUP')
      m.approval = ORDHDR_B.approval
      m.ccontref = ORDHDR_B.ccontref 
      m.Note1 = ORDHDR_B.Note1
      m.Note2 = ORDHDR_B.Note2
	  m.cCurrCode = ORDHDR_B.cCurrCode
	  =SEEK('S'+m.Account+m.Store,'CUSTOMER')
      m.Dist_ctr = Customer.Dist_ctr
      m.Status = ORDHDR_B.Status
      m.priority= ORDHDR_B.Priority
      =SEEK('S' + Scale,'SCALE')
      =SEEK(cWareCode,'WAREHOUS')
      m.cdesc = Warehous.cDesc
      =SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account,'S' + m.Account + m.Store),'Customer')
      m.btname = Customer.BtName
      m.SZ1  = Scale.Sz1
      m.SZ2  = Scale.Sz2
      m.SZ3  = Scale.Sz3
      m.SZ4  = Scale.Sz4
      m.SZ5  = Scale.Sz5
      m.SZ6  = Scale.Sz6
      m.SZ7  = Scale.Sz7
      m.SZ8  = Scale.Sz8
      m.CDIVISION = ORDHDR_B.CDIVISION
      m.REP1  = ORDHDR_B.REP1
      m.REP2  = ORDHDR_B.REP2
      m.DESC  = Style.Desc

      INSERT INTO (lcCursorTemp) FROM MEMVAR
      llHasOrdLine = .T.
    ENDSCAN 
  ENDIF  
ENDSCAN 

RETURN (llHasOrdLine)

*!**************************************************************************
*! Name      : lpGetClrSgStart
*! Developer : Saber Saber (SAB)
*! Date      : 03/07/2011
*! Purpose   : Pro to get Color Segment Start Position
*! Reference : B609543
*!**************************************************************************
PROCEDURE lpGetClrSgStart
  IF (lnClrStart == 0)
    LOCAL laItemSeg, lnColorSegmantLength, lnCount
    lnColorSegmantLength = 0
    DECLARE laItemSeg[1,1]
    lnCount =gfItemMask(@laItemSeg)
    FOR lnCount = 1 TO ALEN(laItemSeg,1)
      IF laItemSeg[lnCount,1]='C'
        lnClrStart = laItemSeg[lnCount,4]
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDPROC
************************************************************
*! Name      : lfFillTempData
*! Developer : Sara Osama
*! Date      : 10/27/2016
*! Purpose   : Fill temp with Data 
************************************************************
FUNCTION lfFillTempData

lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.  
lpGetClrSgStart()

SELECT ORDHDR
=gfSeek('O') 
SCAN REST WHILE cordtype+order ='O' FOR ordhdr.entered <= ldto AND IIF(!EMPTY(ldFrom),Ordhdr.entered >=ldFrom,.T.) AND Status $ 'OH' AND BULK <> 'Y'
  IF !gfSeek(Ordhdr.Account+'O'+'Y'+'O','ORDHDR_B') AND !gfSeek(Ordhdr.Account+'H'+'Y'+'O','ORDHDR_B') 
    LOOP 
  ENDIF
  SELECT ORDLINE
  =gfSeek('O'+ORDHDR.order)
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+ORDHDR.order FOR totqty > 0	
    WAIT WINDOW "Collecting data: Order #"+order+", Style "+style+"." NOWAIT  
  	lcStyleMaj   = SUBSTR(Style,1,lnMajorLen)
  	SCATTER MEMVAR memo
    =SEEK(m.Style,'Style','Style')
    m.ClrCodDsc = gfCodDes(SUBSTR(Style.Style ,lnClrStart, lnColorLen),"COLOR")
    m.StyGrpDsc = gfCodDes(Style.cStyGroup ,'CSTYGROUP')
    m.approval = ORDHDR.approval
    m.ccontref = ordhdr.ccontref 
    m.Note1 = ordhdr.Note1
    m.Note2 = ordhdr.Note2
    m.cCurrCode = ORDHDR.cCurrCode
    =SEEK('S'+m.Account+m.Store,'CUSTOMER')
    m.Dist_ctr = Customer.Dist_ctr
    m.Status = OrdHdr.Status
    m.priority= OrdHdr.Priority
    =SEEK('S' + Scale,'SCALE')
    =SEEK(cWareCode,'WAREHOUS')
    m.cdesc = Warehous.cDesc
    =SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account,'S' + m.Account + m.Store),'Customer')
    m.btname = Customer.BtName
    m.SZ1  = Scale.Sz1
    m.SZ2  = Scale.Sz2
    m.SZ3  = Scale.Sz3
    m.SZ4  = Scale.Sz4
    m.SZ5  = Scale.Sz5
    m.SZ6  = Scale.Sz6
    m.SZ7  = Scale.Sz7
    m.SZ8  = Scale.Sz8
    m.CDIVISION = OrdHdr.CDIVISION
    m.REP1  = OrdHdr.REP1
    m.REP2  = OrdHdr.REP2
    m.DESC  = Style.Desc
 	  m.isBulk = 'No'
    SELECT (lcCursorTemp)
  	APPEND BLANK
  	GATHER MEMVAR memo
  	SELECT ORDLINE
  	llHasRelatedBulk = lfGetAccStyBulk(account,lcStyleMaj)
  	llNonBulkFound = .T.
  	IF !llHasRelatedBulk
   	  SELECT (lcCursorTemp)
  	  DELETE 
  	ENDIF  
  ENDSCAN 
ENDSCAN 
************************************************************
