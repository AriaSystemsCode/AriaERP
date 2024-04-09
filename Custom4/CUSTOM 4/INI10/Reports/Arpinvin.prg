*:***************************************************************************
*: Program file  : ARPINVIN.PRG
*: Program desc. : Custom Invoice form for INTIMELLE INC.
*: TRACK NO      : C201192        
*: System        : Aria4XP
*: Module        : AR
*: Developer     : Mariam Mazhar(MMT)
*:***************************************************************************
* Modifications:
*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[T20091103.0017]
*: B609157,1 MMT 02/28/2010 Fix bug of Wrong Size Qty if invoice has more than one style[T20100217.0004]
*:***************************************************************************

IF !USED('NTA')
  =gfOpenTable('NotePad','NotePad','SH','NTA')
ENDIF 
loogScroll.cCROrientation = 'L'

*Style Major Length
STORE 0 TO lnScaLnGl ,lnScaPosGl ,lnClrLnGl  ,lnClrPosGL 
lnStyMaj = LEN(gfItemMask('PM','', "0001"))
lnScaleWidth = gfGetMemVar('M_EXTWIDTH', oAriaApplication.ActiveCompanyID)  && Scale  Width
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  llarpinv = .F.
  RETURN .F.
ENDIF

lcTmpInvhdr  = gfTempName()
SELECT INVHDR
DIMENSION laInvhdStr[1,18]
lnInvhdrlen = AFIELDS(laInvhdStr)
DIMENSION laInvhdStr[lnInvhdrlen ,18]
=gfCrtTmp(lcTmpInvhdr  ,@laInvhdStr,"INVOICE",lcTmpInvhdr  ,.F.)


SELECT INVLINE
DIMENSION laInvLStr[1,18]
lnInvLlen = AFIELDS(laInvLStr)

*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
*DIMENSION laInvLStr[lnInvLlen + 24,18]
DIMENSION laInvLStr[lnInvLlen + 25,18]
*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[End]

laInvLStr[lnInvLlen +1,1] = 'cStyMaj'
laInvLStr[lnInvLlen +1,2] = 'C'
laInvLStr[lnInvLlen +1,3] = lnStyMaj 
laInvLStr[lnInvLlen +1,4] = 0

laInvLStr[lnInvLlen +2,1] = 'CScale'
laInvLStr[lnInvLlen +2,2] = 'C'
laInvLStr[lnInvLlen +2,3] = 3 
laInvLStr[lnInvLlen +2,4] = 0

laInvLStr[lnInvLlen +3,1] = 'Color'
laInvLStr[lnInvLlen +3,2] = 'C'
laInvLStr[lnInvLlen +3,3] = lnClrLnGl   
laInvLStr[lnInvLlen +3,4] = 0

laInvLStr[lnInvLlen +4,1] = 'StyDesc'
laInvLStr[lnInvLlen +4,2] = 'C'
laInvLStr[lnInvLlen +4,3] = 20
laInvLStr[lnInvLlen +4,4] = 0

lnInc = 5
FOR lnB = 1 TO 14
  laInvLStr[lnInvLlen +lnInc,1] = 'SIZ'+ALLTRIM(STR(lnB))
  laInvLStr[lnInvLlen +lnInc,2] = 'C'
  laInvLStr[lnInvLlen +lnInc,3] = 5
  laInvLStr[lnInvLlen +lnInc,4] = 0
  lnInc = lnInc + 1
ENDFOR 

FOR lnB = 9 TO 14 
  laInvLStr[lnInvLlen +lnInc,1] = 'Qty'+ALLTRIM(STR(lnB))
  laInvLStr[lnInvLlen +lnInc,2] = 'N'
  laInvLStr[lnInvLlen +lnInc,3] = 6
  laInvLStr[lnInvLlen +lnInc,4] = 0
  lnInc = lnInc + 1
ENDFOR 


*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
*FOR lnA = 1 TO 24
laInvLStr[lnInvLlen +lnInc,1] = 'cStyQty'
laInvLStr[lnInvLlen +lnInc,2] = 'C'
laInvLStr[lnInvLlen +lnInc,3] = 56
laInvLStr[lnInvLlen +lnInc,4] = 0
FOR lnA = 1 TO 25
*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[End]
  STORE ' ' TO  laInvLStr[lnInvLlen+LnA,7],laInvLStr[lnInvLlen+LnA,8],;
              	laInvLStr[lnInvLlen+LnA,9],laInvLStr[lnInvLlen+LnA,10],;
	            laInvLStr[lnInvLlen+LnA,11],laInvLStr[lnInvLlen+LnA,12],;
    	        laInvLStr[lnInvLlen+LnA,13],laInvLStr[lnInvLlen+LnA,14],;
        	    laInvLStr[lnInvLlen+LnA,15],laInvLStr[lnInvLlen+LnA,16]
  STORE 0 TO	laInvLStr[lnInvLlen+LnA,17] ,laInvLStr[lnInvLlen+LnA,18]
ENDFOR 
lcTmpInvLine = gfTempName()
=gfCrtTmp(lcTmpInvLine,@laInvLStr,"INVOICE+CSCALE+STYLE",lcTmpInvLine,.F.)

SELECT INVHDR
SET RELATION TO 
LOCATE 
SCAN FOR  &lcRpExp
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcTmpInvhdr) FROM MEMVAR 
  lcIvoiceNum = invhdr.invoice
  SELECT Invline
  =SEEK(invhdr.invoice)
  DIMENSION laScales[1]
  STORE '' TO laScales
  
  SCAN REST WHILE INVOICE+STR(LINENO,6) = lcIvoiceNum 
    
    lcInScale = LEFT(Invline.Scale,lnScaleWidth)
    IF ASCAN(laScales,lcInScale) > 0
      LOOP 
    ENDIF 
    STORE '' TO m.SIZ1,m.SIZ2,m.SIZ3,m.SIZ4,m.SIZ5,m.SIZ6,m.SIZ7,m.SIZ8,m.SIZ9,m.SIZ10,m.SIZ11,m.SIZ12,m.SIZ13,m.SIZ14
    STORE 0 TO m.TotQty,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty9,m.Qty10,m.Qty11,m.Qty12,m.Qty13,m.Qty14
    *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
    m.cStyQty = ''
    *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[End]
    m.CScale =lcInScale
    SELECT Scale
    =SEEK('S'+lcInScale)
    lnCntScl = 1
    SELECT Invline
    lnRecNum = RECNO()
    lnSclCountr = 1
    SELECT Scale 
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcInScale
      IF ASCAN(laScales,lcInScale) = 0
        IF EMPTY(laScales[1])
          laScales[1] = Scale.Scale
        ELSE
          DIMENSION laScales[ALEN(laScales,1)+1]
          laScales[ALEN(laScales,1)] = Scale.Scale
        ENDIF 
      ENDIF 
      FOR lnCs = 1 TO Scale.cnt
        lcCntScl = ALLTRIM(STR(lnCntScl))
        lcB = STR(lnCs ,1)
        m.SIZ&lcCntScl = Scale.Sz&lcB.
        lnCntScl = lnCntScl + 1
        IF lnCntScl > 14
          EXIT 
        ENDIF 
      ENDFOR
      IF lnCntScl > 14
        EXIT 
      ENDIF 
    ENDSCAN 
    SELECT Invline 
    SCAN REST WHILE INVOICE+STR(LINENO,6) = lcIvoiceNum FOR Scale = lcInScale
      lcStyleClr = SUBSTR(Invline.Style ,1,lnClrLnGl+lnClrPosGL-1)
      IF SEEK(Invline.INVOICE+PADR(lcInScale,3)+lcStyleClr,lcTmpInvLine)
        LOOP 
      ENDIF 
      SCATTER MEMO MEMVAR
      m.TotQty = 0 
	  *: B609157,1 MMT 02/28/2010 Fix bug of Wrong Size Qty if invoice has more than one style[Start]
   	  STORE 0 TO m.TotQty,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty9,m.Qty10,m.Qty11,m.Qty12,m.Qty13,m.Qty14      
	  *: B609157,1 MMT 02/28/2010 Fix bug of Wrong Size Qty if invoice has more than one style[End]
      m.Color = SUBSTR(Style,lnClrPosGL,lnClrLnGl)
      m.cStyMaj = SUBSTR(Style,1,lnStyMaj)
      m.CScale= lcInScale
      lnStyRec = RECNO()
      SCAN REST WHILE INVOICE+STR(LINENO,6) = lcIvoiceNum FOR Scale = lcInScale AND;
        			  SUBSTR(Invline.Style ,1,lnClrLnGl+lnClrPosGL-1)=lcStyleClr
        =SEEK(Invline.Style,'Style','Style')
        m.StyDesc = Style.Desc
        =SEEK('S'+Invline.Scale,'Scale')
        lnGetFrstScl = 0
        FOR lnCnte = 1 TO scale.Cnt 
          lcCnte = STR(lnCnte ,1)
          FOR lnCntCur = 1 TO 14 
            lcCntCur = ALLTRIM(STR(lnCntCur))
            IF UPPER(Scale.Sz&lcCnte.) = UPPER(m.SIZ&lcCntCur)
              lnGetFrstScl = lnCntCur 
              EXIT 
            ENDIF 
          ENDFOR   
          IF lnGetFrstScl <> 0
            EXIT 
          ENDIF 
        ENDFOR 
        lnStCn = lnGetFrstScl 
        FOR lnInvCnt =1 TO SCALE.CNT
          lcStCn = ALLTRIM(STR(lnStCn))
          lcInvCnt = STR(lnInvCnt ,1)
           m.Qty&lcStCn = Invline.Qty&lcInvCnt.
           m.TotQty = m.TotQty + m.Qty&lcStCn
           lnStCn = lnStCn + 1
        ENDFOR 
        *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
	 	m.cStyQty= IIF(m.Qty1 = 0,SPACE(4),STR(m.Qty1,4,0))+;
				  IIF(m.Qty2 = 0,SPACE(4),STR(m.Qty2,4,0))+;	
				  IIF(m.Qty3 = 0,SPACE(4),STR(m.Qty3,4,0))+;
				  IIF(m.Qty4 = 0,SPACE(4),STR(m.Qty4,4,0))+;
				  IIF(m.Qty5 = 0,SPACE(4),STR(m.Qty5,4,0))+;
				  IIF(m.Qty6 = 0,SPACE(4),STR(m.Qty6,4,0))+;
				  IIF(m.Qty7 = 0,SPACE(4),STR(m.Qty7,4,0))+;
				  IIF(m.Qty8 = 0,SPACE(4),STR(m.Qty8,4,0))+;
				  IIF(m.Qty9 = 0,SPACE(4),STR(m.Qty9,4,0))+;
				  IIF(m.Qty10 = 0,SPACE(4),STR(m.Qty10,4,0))+;
				  IIF(m.Qty11 = 0,SPACE(4),STR(m.Qty11,4,0))+;
				  IIF(m.Qty12 = 0,SPACE(4),STR(m.Qty12,4,0))+;
				  IIF(m.Qty13 = 0,SPACE(4),STR(m.Qty13,4,0))+;
				  IIF(m.Qty14 = 0,SPACE(4),STR(m.Qty14,4,0))
	  *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[End]
      ENDSCAN   
      SELECT Invline 
      IF BETWEEN(lnStyRec ,1,RECCOUNT())
        GO RECORD lnStyRec 
      ENDIF 
      INSERT INTO (lcTmpInvLine) FROM MEMVAR  
    ENDSCAN 
    SELECT Invline 
    IF BETWEEN(lnRecNum ,1,RECCOUNT())
      GO RECORD lnRecNum 
    ENDIF 
    
  ENDSCAN 
ENDSCAN 


SELECT(lcTmpInvhdr)
SET RELATION TO IIF(EMPTY(&lcTmpInvhdr..Store) OR &lcTmpInvhdr..Store = "********",'M' + &lcTmpInvhdr..Account,'S' + &lcTmpInvhdr..Account + &lcTmpInvhdr..Store) INTO CUSTOMER ADDITIVE
SET RELATION TO &lcTmpInvhdr..INVOICE INTO Invhdr ADDITIVE
SET RELATION TO 'O' + &lcTmpInvhdr..order INTO Ordhdr ADDITIVE
SET RELATION TO &lcTmpInvhdr..INVOICE INTO (lcTmpInvLine) ADDITIVE  
SET SKIP TO (lcTmpInvLine) 
SELECT(lcTmpInvhdr)
LOCATE 
DO gfDispRe WITH EVAL('lcFormName') 

lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)

llarpinv = .F.
SELECT (lcTmpInvhdr) 
SET RELATION TO 

*!*************************************************************
*! Name      : lfGetLastLine
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/24/2008
*! Purpose   : get last line in invoice
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTmpInvLine)
lnThRec = RECNO()    && Save Current record #.
LNTOTAMT = 0


=SEEK(&lcTmpInvhdr..Invoice)
SCAN REST WHILE INVOICE+CSCALE+STYLE =  &lcTmpInvhdr..Invoice
  LcLstSty = cStyMaj+Color 
  LNTOTAMT = LNTOTAMT + TotQty * Price
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT(lcTmpInvLine))
  GO lnThRec IN (lcTmpInvLine)   && Restore Record #
ELSE
  GO TOP IN (lcTmpInvLine)   && Restore Record #
ENDIF
SELECT (lcThAlias)            && Restore Alias.
RETURN ''


*!*************************************************************
*! Name      : lfSolAdrSp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/24/2008
*! Purpose   : get sold and ship to addresses
*!*************************************************************
FUNCTION lfSolAdrSp
PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

DECLARE laFactor[5,1]
STORE '' TO laFactor,lcFacName   

*-- Fill laFactor with factor address
IF !EMPTY(&lcTmpInvhdr..CFACCODE)
  =SEEK(&lcTmpInvhdr..CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF
llEndGroup = .F.

lcShipVia = gfCodDes(&lcTmpInvhdr..ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(&lcTmpInvhdr..cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')


SELECT (lcTmpInvhdr)
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !ORDHDR.lStrDirct
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(&lcTmpInvhdr..STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(&lcTmpInvhdr..STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTmpInvhdr)
RETURN ''