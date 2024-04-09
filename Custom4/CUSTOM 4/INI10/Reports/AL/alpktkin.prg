*:***************************************************************************
*: Program file  : ARPINVIN.PRG
*: Program desc. : Custom Pick ticket form for INTIMELLE INC.
*: TRACK NO      : C201192        
*: System        : Aria4XP
*: Module        : AL
*: Developer     : Mariam Mazhar(MMT)
*:***************************************************************************
* Modifications:
*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[T20091103.0017]
*: B609157,1 MMT 03/02/2010 Fix bug of Wrong Size Qty if PIKTKT has more than one style[T20100217.0004]
*:B609827,1 MMT 02/09/2012 Change the Export Custom Pick ticket form IN to excel format[T20120112.0012]
*:***************************************************************************
*Style Major Length
loogScroll.cCROrientation = 'L'
IF !USED('NTA')
  =gfOpenTable('NotePad','NotePad','SH','NTA')
ENDIF 
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
SELECT (lcTmpOrdL)
lcTmpPik = gfTempName()
DIMENSION laInvLStr[1,18]
lnInvLlen = AFIELDS(laInvLStr)
*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
*DIMENSION laInvLStr[lnInvLlen + 30,18]
DIMENSION laInvLStr[lnInvLlen + 31,18]
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

*!*  laInvLStr[lnInvLlen +5,1] = 'TotalQt'
*!*  laInvLStr[lnInvLlen +5,2] = 'N'
*!*  laInvLStr[lnInvLlen +5,3] = 7
*!*  laInvLStr[lnInvLlen +5,4] = 0

*!*  laInvLStr[lnInvLlen +6,1] = 'TotOpn'
*!*  laInvLStr[lnInvLlen +6,2] = 'N'
*!*  laInvLStr[lnInvLlen +6,3] = 7
*!*  laInvLStr[lnInvLlen +6,4] = 0

*!*  lnInc = 7
lnInc = 5
lnCntQty = 9
FOR lnX = 1 TO 6
  laInvLStr[lnInvLlen +lnInc,1] = 'qty'+ALLTRIM(STR(lnCntQty))
  laInvLStr[lnInvLlen +lnInc,2] = 'N'
  laInvLStr[lnInvLlen +lnInc,3] = 6
  laInvLStr[lnInvLlen +lnInc,4] = 0
  lnInc = lnInc + 1
  lnCntQty = lnCntQty + 1
ENDFOR 



FOR lnB = 1 TO 14
  laInvLStr[lnInvLlen +lnInc,1] = 'SIZ'+ALLTRIM(STR(lnB))
  laInvLStr[lnInvLlen +lnInc,2] = 'C'
  laInvLStr[lnInvLlen +lnInc,3] = 5
  laInvLStr[lnInvLlen +lnInc,4] = 0
  lnInc = lnInc + 1
ENDFOR 

FOR lnB = 9 TO 14 
  laInvLStr[lnInvLlen +lnInc,1] = 'Pik'+ALLTRIM(STR(lnB))
  laInvLStr[lnInvLlen +lnInc,2] = 'N'
  laInvLStr[lnInvLlen +lnInc,3] = 6
  laInvLStr[lnInvLlen +lnInc,4] = 0
  lnInc = lnInc + 1
ENDFOR 

*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
*FOR lnA = 1 TO 30
laInvLStr[lnInvLlen +lnInc,1] = 'cQtySz'
laInvLStr[lnInvLlen +lnInc,2] = 'C'
laInvLStr[lnInvLlen +lnInc,3] = 56
laInvLStr[lnInvLlen +lnInc,4] = 0
FOR lnA = 1 TO 31
*: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[End]
  STORE ' ' TO  laInvLStr[lnInvLlen+LnA,7],laInvLStr[lnInvLlen+LnA,8],;
              	laInvLStr[lnInvLlen+LnA,9],laInvLStr[lnInvLlen+LnA,10],;
	            laInvLStr[lnInvLlen+LnA,11],laInvLStr[lnInvLlen+LnA,12],;
    	        laInvLStr[lnInvLlen+LnA,13],laInvLStr[lnInvLlen+LnA,14],;
        	    laInvLStr[lnInvLlen+LnA,15],laInvLStr[lnInvLlen+LnA,16]
  STORE 0 TO	laInvLStr[lnInvLlen+LnA,17] ,laInvLStr[lnInvLlen+LnA,18]
ENDFOR 

=gfCrtTmp(lcTmpPik ,@laInvLStr,"PIKTKT+CSCALE+STYLE",lcTmpPik ,.F.)
SELECT (lcTmpOrdL)
SET RELATION TO 
lcKeyId = KEY()
DIMENSION laScales[1]
STORE '' TO laScales
lcOldPik = ''
LOCATE 
SCAN FOR !EMPTY(Style)
  IF lcOldPik  <> &lcTmpOrdL..Piktkt
    STORE '' TO laScales
  ENDIF 
  lcInScale = LEFT(&lcTmpOrdL..Scale,lnScaleWidth)
  IF ASCAN(laScales,lcInScale) > 0
    LOOP 
  ENDIF 
  STORE '' TO m.SIZ1,m.SIZ2,m.SIZ3,m.SIZ4,m.SIZ5,m.SIZ6,m.SIZ7,m.SIZ8,m.SIZ9,m.SIZ10,m.SIZ11,m.SIZ12,m.SIZ13,m.SIZ14
  STORE 0 TO m.TotQty,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty9,m.Qty10,m.Qty11,m.Qty12,m.Qty13,m.Qty14
  STORE 0 TO m.TotPik,m.PIK1,m.PIK2,m.PIK3,m.PIK4,m.PIK5,m.PIK6,m.PIK7,m.PIK8,m.PIK9,m.PIK10,m.PIK11,m.PIK12,m.PIK13,m.PIK14
  *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
  m.cQtySz = ''
  *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[End]
  m.CScale =lcInScale
  SELECT Scale
  =SEEK('S'+lcInScale)
  lnCntScl = 1
  SELECT (lcTmpOrdL)
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
  SELECT (lcTmpOrdL)
  lcPikT = &lcTmpOrdL..Piktkt
  SCAN REST WHILE EVALUATE(lcKeyId)  =lcPikT FOR  Scale = lcInScale AND !EMPTY(Style)
    lcStyleClr = SUBSTR(&lcTmpOrdL..Style ,1,lnClrLnGl+lnClrPosGL-1)
    IF SEEK(&lcTmpOrdL..Piktkt+PADR(lcInScale,3)+lcStyleClr,lcTmpPik)
      LOOP 
    ENDIF 
    SCATTER MEMO MEMVAR
	*: B609157,1 MMT 03/02/2010 Fix bug of Wrong Size Qty if PIKTKT has more than one style[Start]  
	STORE 0 TO m.TotQty,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty9,m.Qty10,m.Qty11,m.Qty12,m.Qty13,m.Qty14	  
	*: B609157,1 MMT 03/02/2010 Fix bug of Wrong Size Qty if PIKTKT has more than one style[End]    	
    m.TotQty = 0 
    m.TotPick = 0
    m.Color = SUBSTR(Style,lnClrPosGL,lnClrLnGl)
    m.cStyMaj = SUBSTR(Style,1,lnStyMaj)
    m.CScale= lcInScale
    lnStyRec = RECNO()
    SCAN REST WHILE  EVALUATE(lcKeyId) =lcPikT  FOR Scale = lcInScale AND;
      			  SUBSTR(&lcTmpOrdL..Style ,1,lnClrLnGl+lnClrPosGL-1)=lcStyleClr AND !EMPTY(Style)
      =SEEK(&lcTmpOrdL..Style,'Style','Style')
      m.StyDesc = Style.Desc
      =SEEK('S'+&lcTmpOrdL..Scale,'Scale')
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
         m.PIK&lcStCn = &lcTmpOrdL..Pik&lcInvCnt.
         m.Qty&lcStCn = &lcTmpOrdL..Qty&lcInvCnt.
         m.TOTPICK = m.TOTPICK + m.Pik&lcStCn
         m.TotQty = m.TotQty + m.Qty&lcStCn
         lnStCn = lnStCn + 1
      ENDFOR 
      *: B609081,1 MMT 11/10/2009 Fix bug of *** if number more 3 digits[Start]
	  m.cQtySz = IIF(m.Qty1 = 0,SPACE(4),STR(m.Qty1,4,0))+;
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
    SELECT (lcTmpOrdL) 
    IF BETWEEN(lnStyRec ,1,RECCOUNT())
      GO RECORD lnStyRec 
    ENDIF 
    INSERT INTO (lcTmpPik) FROM MEMVAR  
  ENDSCAN 
  SELECT (lcTmpOrdL) 
  IF BETWEEN(lnRecNum ,1,RECCOUNT())
    GO RECORD lnRecNum 
  ENDIF 
  lcOldPik = &lcTmpOrdL..Piktkt
ENDSCAN 


SELECT (lcTmpPik) 
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH

SET RELATION TO PikTkt INTO &lcPiktktTemp ADDITIVE
SET RELATION TO 'O' + Order INTO &lcOrdHdr ADDITIVE

*--IF We are to Print Order Lines Note Pad
IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO &lcOrdLnTmp ADDITIVE
ENDIF    && End of IF

SET RELATION TO Style INTO &lcStyleFile ADDITIVE
SET RELATION TO 'S' + Scale INTO &lcScaleFile ADDITIVE
SELECT(lcPiktktTemp)
SET RELATION TO cWareCode INTO &lcWareHous
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO &lcCustomer ADDITIVE

SELECT (lcTmpPik) 
LOCATE 
*:B609827,1 MMT 02/09/2012 Change the Export Custom Pick ticket form IN to excel format[T20120112.0012][Start]
IF (oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL")
  SELECT (lcTmpOrdL)
  LOCATE
ENdIF
*!*	*:B609827,1 MMT 02/09/2012 Change the Export Custom Pick ticket form IN to excel format[T20120112.0012][End]
DO gfDispRe WITH EVAL('lcFormName')                     
llAlpktk = .F.

*!*************************************************************
*! Name      : lfGetLastLine
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/24/2008
*! Purpose   : get last line in invoice
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTmpPik)
lnThRec = RECNO()    && Save Current record #.
lNTOTPIK = 0
lnTOTQT  = 0
lcPikTkt = &lcTmpPik..Piktkt
lcKeyFld = KEY()
=SEEK(lcPikTkt)
SCAN REST WHILE EVALUATE(lcKeyFld)=  lcPikTkt 
  LcLstSty = cStyMaj+Color 
  LNTOTPIK = LNTOTPIK + TotPik
  lnTOTQT  = lnTOTQT + TOTQTY 
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT(lcTmpPik))
  GO lnThRec IN (lcTmpPik)   && Restore Record #
ELSE
  GO TOP IN (lcTmpPik)   && Restore Record #
ENDIF
SELECT (lcThAlias)            && Restore Alias.
RETURN ''


*!*************************************************************
*! Name      : lfSolAdrSp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/24/2008
*! Purpose   : get sold and ship to addresses
*!*************************************************************
*
FUNCTION lfSolAdrSp
PRIVATE lcDistCntr

llEndGroup = .F.
=gfRltFld(EVALUATE(lcORDHDR+'.cDivision') , @laDivLName , 'CDIVISION')

SELECT(lcCUSTOMER)
SEEK IIF(EMPTY(&lcPiktktTemp..STORE),'M','S')+ &lcPiktktTemp..Account + &lcPiktktTemp..STORE
lcShipVia = gfCodDes(IIF(&lcCUSTOMER..nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > &lcCUSTOMER..nBrkWeight ,;
                         &lcCUSTOMER..cAltShpvia ,IIF(&lcORDHDR..ShipVia ='*',&lcCUSTOMER..ShipVia,&lcORDHDR..ShipVia)), 'SHIPVIA')

lcSeason = gfCodDes(&lcORDHDR..Season , 'SEASON')
lcSpcInst = gfCodDes(&lcORDHDR..SpcInst , 'SPCINST')
lcTerms = gfCodDes(&lcORDHDR..cTermCode , 'CTERMCODE')

SELECT(lcCUSTOMER)
SEEK IIF(EMPTY(&lcPiktktTemp..Store) , 'M' , 'S') + &lcPiktktTemp..Account + &lcPiktktTemp..Store

lcSolTName = BTName
lcShpTName = IIF(&lcORDHDR..Alt_ShpTo , &lcORDHDR..STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 4 , '2')
laSoldTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(&lcCustomer..Phone1 , '@R '+lcPhonPict)
=lfAdrShift('laSoldTo')

IF &lcORDHDR..Alt_ShpTo

  SELECT (lcORDHDR)
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5

ELSE    && Else
  SELECT(lcCUSTOMER)
  lcDistCntr = &lcCUSTOMER..Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr) AND !(&lcORDHDR..lStrDirct)
    SEEK 'S' + &lcPiktktTemp..Account + lcDistCntr
  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr(lcCUSTOMER , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(&lcCustomer..Phone1 ,'@R '+lcPhonPict)
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTmpPik)
RETURN ''
*-- end of lfSolSpAdr.