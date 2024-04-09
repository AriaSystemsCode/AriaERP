*:***************************************************************************
*: Program file  : ALPKTKE.PRG
*: Program desc. : ALLOCATION PIKTICKT FORM "E".
*: Date          : 28/09/2006
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION(AL)
*: Developer     : Mariam Mazhar [MMT]
*: Tracking Job Number: 
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKE
*:***************************************************************************
*: Modifications :
*:	B608021,1 MMT 03/28/07 fix bug of wrong ship to address in Form E 
*:***************************************************************************
SELECT (lcTmpOrdL)
lcOldKey = ORDER()
INDEX ON PikTkt + Order +IIF(PIKTKT = "******",ORDLINE.CWARECODE,'') + cGrupDetal + STR(LineNo , 6) ;
	 TAG NewIndex 

loOgScroll.lcOGLastForm = 'ALPKTKE'
loOgScroll.cCRorientation = 'P'
loOgScroll.lcLogoPath = ''

lcTempFile = loogscroll.gfTempName()

=lfCrttemp(lcTempFile)

STORE 0 TO lnClrLnCd , lnClrPosCd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnE  = LEN(laItemSeg[lnCount,3])
    lnClrPosE = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lcCustPo = ""
lcPrtAdr = IIF(TYPE("lcPrtAdr")="C",lcPrtAdr,"C")
XPIKNAME     =  lcPrtAdr # "N"

PRTPRICE   = llRpStyPrc                   && variable hold the printing of he style price
lcPrnStLoc = llRpStyLoc                   && for the style location
lcPrnSkPck = llRpSkuBck                   && for the sku\pack
lcPrnSkSiz = llRpSkuSiz                   && for the sku\by size 

PRTNPAD = IIF(llRpOrdLNt , 'Y' , 'N')


STORE 0.00 TO XORDTOT, XVALUE , SVALUE , XTOTQTY
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4

XPHONE = lcXphone

HLINE5 = XPHONE
*will sent as papameters
MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3

IF lcPrtAdr = "C"
  IF LEN(HLINE3) = 0
    STORE HLINE4 TO HLINE3
    STORE HLINE5 TO HLINE4
    STORE ' '    TO HLINE5
  ENDIF
ENDIF

*--Get Memorey Variables[Start]
XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
*--Get Memorey Variables[End]

SELECT (lcTmpOrdL)
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

lcStrToPrn=' '
lcSkuSize =' '
lcSclStrn =' '
lnLen     = 0
lcStr     =' '
NEWDOC  = .T.
SCAN FOR !EMPTY(Style)
  SELECT (lcTmpOrdL)
  lcWareH = &lcOrdLnTmp..CWareCode     &&variable to hold warehouse code
  IF lcPrtAdr = "W"
    IF PIKTKT = "******"     && just for ALLOCATED lines
      =SEEK(&lcOrdLnTmp..CWARECODE,lcWareHous)
    ENDIF
    m.HLINE2     = gfGetAdr(lcWareHous , '' , '' , '' , 1)
    m.HLINE3     = gfGetAdr(lcWareHous , '' , '' , '' , 2)
    m.HLINE4     = gfGetAdr(lcWareHous , '' , '' , '' , 3)
    m.HLINE5     = TRANSFORM(&lcWareHous..CPHONE,'@R '+lcPhonPict)
    IF LEN(HLINE3) = 0
      STORE m.HLINE4 TO m.HLINE3
      STORE m.HLINE5 TO m.HLINE4
      STORE ' '      TO m.HLINE5
    ENDIF
  ENDIF
  m.XPIKTKT = PIKTKT
  m.XORDER  = ORDER
  m.XDATE   = PIKDATE
  m.cDept   = &lcOrdHdr..DEPT
  m.XSTORE  = &lcPiktktTemp..STORE
  SELECT (lcOrdHdr)
  XMULTI   = MULTI
  m.XACCOUNT = ACCOUNT
  STORE 0.00 TO XORDTOT, m.XVALUE , SVALUE
  m.cCustPo = IIF(&lcOrdHdr..MultiPO,&lcTmpOrdL..CustPO,&lcOrdHdr..CustPO)
  m.XBTNAME  = &lcCustomer..BTNAME
  m.XBTADDR1 = gfGetAdr(lcCustomer , '' , '' , '' , 1 , '2') 
  m.XBTADDR2 = gfGetAdr(lcCustomer , '' , '' , '' , 2 , '2') 
  m.XBTADDR3 = gfGetAdr(lcCustomer , '' , '' , '' , 3 , '2')
  IF LEN(TRIM(m.XBTADDR2)) =0
    m.XBTADDR2 = m.XBTADDR3
    m.XBTADDR3 = ''
  ENDIF
  m.XTERM_DATA = gfCodDes(&lcOrdHdr..CTERMCODE , 'CTERMCODE')
  m.XSHIP_DATA = gfCodDes(&lcOrdHdr..SHIPVIA , 'SHIPVIA')
  m.XSPCI_DATA = gfCodDes(&lcOrdHdr..SPCINST , 'SPCINST')
  m.XSEAS_DATA  = gfCodDes(&lcOrdHdr..SEASON , 'SEASON')
  m.XDIVI_DATA  = gfCodDes(&lcOrdHdr..CDIVISION , 'CDIVISION')
  SELECT (lcTmpOrdL)
  IF PIKTKT = "******"
    =SEEK(&lcOrdLnTmp..CWARECODE,lcWareHous)
  ENDIF
  m.HLINE1 = IIF(EMPTY(lcDivLName) ,IIF(lcPrtAdr = "W", SUBSTR(&lcWareHous..CDESC,1,30) , lcCompName) , lcDivLName)
  STORE "" TO m.XSTNAME,m.XSTADDR1,m.XSTADDR2,m.XSTADDR3,m.XSTADDR4
  m.XPIECES = 0
  SELECT (lcTmpOrdL)
  SELECT (lcOrdHdr)
  lcDistCntr = ""
  IF Alt_ShpTo
    lcShpTName  = &lcOrdHdr..STName   
    laShipTo[1] = &lcOrdHdr..cAddress1
    laShipTo[2] = &lcOrdHdr..cAddress2
    laShipTo[3] = &lcOrdHdr..cAddress3
    laShipTo[4] = &lcOrdHdr..cAddress4
    laShipTo[5] = &lcOrdHdr..cAddress5

    m.XSTNAME  = lcShpTName
    m.XSTADDR1 = laShipTo[1]
    m.XSTADDR2 = laShipTo[2]
    m.XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(m.XSTADDR2)) =0
      m.XSTADDR2 = m.XSTADDR3
      m.XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT(lcCustomer)
    lnRecNo = RECNO()       && Variable to hold the record number.
    *B608021,1 MMT 03/28/07 fix bug of wrong ship to address in Form E [Start]
    *IF !EMPTY(Dist_Ctr)    
    IF !EMPTY(Dist_Ctr) AND !(&lcOrdHdr..lStrDirct)
    *B608021,1 MMT 03/28/07 fix bug of wrong ship to address in Form E [End]
      lcOrgnalSt = &lcCustomer..STORE
      lcDistCntr = ALLTRIM(Dist_Ctr)
      =SEEK('S'+m.XACCOUNT+lcDistCntr)
      lcDistCntr = lcOrgnalSt
    ENDIF 
    m.XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    m.XSTADDR1 = gfGetAdr(lcCustomer , '' , '' , '' , 1 ) 
    m.XSTADDR2 = gfGetAdr(lcCustomer , '' , '' , '' , 2 ) 
    m.XSTADDR3 = gfGetAdr(lcCustomer , '' , '' , '' , 3 )
    IF !EMPTY(lcDistCntr) 
      GO lnRecNo        
    ENDIF         
    IF LEN(TRIM(m.XSTADDR2)) =0
      m.XSTADDR2 = m.XSTADDR3
      m.XSTADDR3 = ''
    ENDIF
  ENDIF  
  SELECT (lcOrdHdr)
	m.HLINE1 = IIF(XPIKNAME=.T.,m.HLINE1,'')            
	m.HLINE2 = IIF(XPIKNAME=.T.,m.HLINE2,'')            
	m.HLINE3 = IIF(XPIKNAME=.T.,m.HLINE3,'')            
	m.HLINE4 = IIF(XPIKNAME=.T.,m.HLINE4,'')            
	m.XDIVI_DATA = SUBSTR(ALLTRIM(m.XDIVI_DATA),1,11)
	m.APPROVAL = &lcORDHDR..APPROVAL
	m.XSTORE   = IIF(EMPTY(lcDistCntr),m.XSTORE,lcDistCntr)
	m.XSHIP_DATA = SUBSTR(m.XSHIP_DATA,1,10) 
	m.rep1 = &lcORDHDR..REP1
	m.rep2 = &lcORDHDR..REP2
	m.ENTERED = SUBSTR(DTOC(&lcORDHDR..ENTERED),1,5)
	m.START =  SUBSTR(DTOC(&lcORDHDR..START),1,5)
	m.COMPLETE = &lcORDHDR..COMPLETE
	m.XTERM_DATA = SUBSTR(XTERM_DATA,1,11)                

	m.sz1 = PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ1),1,3),3,' ')
	m.sz2 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ2),1,3),3,' ')
	m.sz3 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ3),1,3),3,' ')
	m.sz4 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ4),1,3),3,' ')
	m.sz5 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ5),1,3),3,' ')
	m.sz6 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ6),1,3),3,' ')
	m.sz7 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ7),1,3),3,' ')
	m.sz8 =PADL(SUBSTR(ALLTRIM(&lcScaleFile..SZ8),1,3),3,' ')

  SELECT (lcTmpOrdL)
  m.cClrDesc = gfCodDes(SUBSTR(&lcStyleFile..Style,lnClrPosE,lnClrLnE) ,'COLOR')	
  SELECT (lcTmpOrdL)
  m.STYLE = STYLE
  m.XStyDesc = &lcStyleFile..Desc
  m.Scale = ALLTRIM(&lcStyleFile..Scale)
  m.PIK1 = PIK1
  m.PIK2 = PIK2
  m.PIK3 = PIK3
  m.PIK4 = PIK4
  m.PIK5 = PIK5   
  m.PIK6 = PIK6
  m.PIK7 = PIK7
  m.PIK8 = PIK8
  m.TOTPIK = TOTPIK
  IF PRTPRICE
    m.PRICE = PRICE
  ENDIF
  XTOTQTY = XTOTQTY+ TOTQTY
  m.XVALUE  = XVALUE + TOTPIK * PRICE
  m.XPIECES = m.XPIECES + TOTPIK
  m.MNOTES = ''
  m.lhasnotes = 0
  IF loNotePad.SEEK('B' + &lcOrdHdr..Order)
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    m.MNOTES= &lcTempNotePad..MNOTES
    m.lhasnotes = IIF(!EMPTY(m.MNOTES),1,0)
  ENDIF  
  m.Note1 = &lcordhdr..Note1
  m.Note2 = &lcordhdr..Note2

  INSERT INTO (lcTempFile) FROM MEMVAR 
ENDSCAN 


DIMENSION loOgScroll.laCRParams[6,2]

loOgScroll.laCRParams[1,1] = 'PRTPRICE'
loOgScroll.laCRParams[1,2] = IIF(PRTPRICE,1,0)

loOgScroll.laCRParams[2,1] = 'MSGL1'
loOgScroll.laCRParams[2,2] = MSGL1 

loOgScroll.laCRParams[3,1] = 'MSGL2'
loOgScroll.laCRParams[3,2] = MSGL2

loOgScroll.laCRParams[4,1] = 'MSGL3'
loOgScroll.laCRParams[4,2] =  MSGL3

loOgScroll.laCRParams[5,1] = 'llRpPrtClr'
loOgScroll.laCRParams[5,2] = IIF(llRpPrtClr ,1,0)

loOgScroll.laCRParams[6,1] = 'llRpOrdLNt'
loOgScroll.laCRParams[6,2] = IIF(llRpOrdLNt And llRpOrdNot,1,0)



DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempFile+ ".DBF"

SELECT (lcTempFile)
USE
*-----------
gfDispRe()
RETURN 

FUNCTION lfCrttemp
PARAMETERS lcTemp

DIMENSION laFileStru[57,4]
laFileStru[1,1] = 'STYLE'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 19
laFileStru[1,4] = 0

laFileStru[2,1] = 'Scale'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 1
laFileStru[2,4] = 0

laFileStru[3,1] = 'PIK1'
laFileStru[3,2] = 'N'
laFileStru[3,3] = 4
laFileStru[3,4] = 0

laFileStru[4,1] = 'PIK2'
laFileStru[4,2] = 'N'
laFileStru[4,3] = 4
laFileStru[4,4] = 0

laFileStru[5,1] = 'PIK3'
laFileStru[5,2] = 'N'
laFileStru[5,3] = 4
laFileStru[5,4] = 0

laFileStru[6,1] = 'PIK4'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 4
laFileStru[6,4] = 0

laFileStru[7,1] = 'PIK5'
laFileStru[7,2] = 'N'
laFileStru[7,3] = 4
laFileStru[7,4] = 0

laFileStru[8,1] = 'PIK6'
laFileStru[8,2] = 'N'
laFileStru[8,3] = 4
laFileStru[8,4] = 0

laFileStru[9,1] = 'PIK7'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 4
laFileStru[9,4] = 0

laFileStru[10,1] = 'PIK8'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 4
laFileStru[10,4] = 0

laFileStru[11,1] = 'TOTPIK'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 5
laFileStru[11,4] = 0

laFileStru[12,1] = 'PRICE'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 7
laFileStru[12,4] = 2

laFileStru[13,1] = 'XStyDesc'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 20
laFileStru[13,4] = 0

laFileStru[14,1] = 'cClrDesc'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 30
laFileStru[14,4] = 0

laFileStru[15,1] = 'MNOTES'
laFileStru[15,2] = 'M'
laFileStru[15,3] = 10
laFileStru[15,4] = 0

laFileStru[16,1] = 'HLINE1'
laFileStru[16,2] = 'C'
laFileStru[16,3] = 100
laFileStru[16,4] = 0

laFileStru[17,1] = 'HLINE2'
laFileStru[17,2] = 'C'
laFileStru[17,3] = 100
laFileStru[17,4] = 0


laFileStru[18,1] = 'HLINE3'
laFileStru[18,2] = 'C'
laFileStru[18,3] = 100
laFileStru[18,4] = 0

laFileStru[19,1] = 'HLINE4'
laFileStru[19,2] = 'C'
laFileStru[19,3] = 100
laFileStru[19,4] = 0

laFileStru[20,1] = 'HLINE5'
laFileStru[20,2] = 'C'
laFileStru[20,3] = 100
laFileStru[20,4] = 0

laFileStru[21,1] = 'XSTNAME'
laFileStru[21,2] = 'C'
laFileStru[21,3] = 30
laFileStru[21,4] = 0

laFileStru[22,1] = 'XSTADDR1'
laFileStru[22,2] = 'C'
laFileStru[22,3] = 30
laFileStru[22,4] = 0

laFileStru[23,1] = 'XSTADDR2'
laFileStru[23,2] = 'C'
laFileStru[23,3] = 30
laFileStru[23,4] = 0

laFileStru[24,1] = 'XSTADDR3'
laFileStru[24,2] = 'C'
laFileStru[24,3] = 30
laFileStru[24,4] = 0

laFileStru[25,1] = 'cCustPO'
laFileStru[25,2] = 'C'
laFileStru[25,3] = 15
laFileStru[25,4] = 0

laFileStru[26,1] = 'XSTORE'
laFileStru[26,2] = 'C'
laFileStru[26,3] = 10
laFileStru[26,4] = 0

laFileStru[27,1] = 'cDept'
laFileStru[27,2] = 'C'
laFileStru[27,3] = 10
laFileStru[27,4] = 0

laFileStru[28,1] = 'XORDER'
laFileStru[28,2] = 'C'
laFileStru[28,3] = 6
laFileStru[28,4] = 0

laFileStru[29,1] = 'XPIKTKT'
laFileStru[29,2] = 'C'
laFileStru[29,3] = 6
laFileStru[29,4] = 0

laFileStru[30,1] = 'XSHIP_DATA'
laFileStru[30,2] = 'C'
laFileStru[30,3] = 10
laFileStru[30,4] = 0

laFileStru[31,1] = 'XDIVI_DATA'
laFileStru[31,2] = 'C'
laFileStru[31,3] = 10
laFileStru[31,4] = 0


laFileStru[32,1] = 'APPROVAL'
laFileStru[32,2] = 'C'
laFileStru[32,3] = 10
laFileStru[32,4] = 0


laFileStru[33,1] = 'XDATE'
laFileStru[33,2] = 'D'
laFileStru[33,3] = 8
laFileStru[33,4] = 0

laFileStru[34,1] = 'XBTNAME'
laFileStru[34,2] = 'C'
laFileStru[34,3] = 30
laFileStru[34,4] = 0

laFileStru[35,1] = 'XBTADDR1'
laFileStru[35,2] = 'C'
laFileStru[35,3] = 30
laFileStru[35,4] = 0

laFileStru[36,1] = 'XBTADDR2'
laFileStru[36,2] = 'C'
laFileStru[36,3] = 30
laFileStru[36,4] = 0

laFileStru[37,1] = 'XBTADDR3'
laFileStru[37,2] = 'C'
laFileStru[37,3] = 30
laFileStru[37,4] = 0

laFileStru[38,1] = 'XACCOUNT'
laFileStru[38,2] = 'C'
laFileStru[38,3] = 5
laFileStru[38,4] = 0

laFileStru[39,1] = 'REP1'
laFileStru[39,2] = 'C'
laFileStru[39,3] = 3
laFileStru[39,4] = 0

laFileStru[40,1] = 'REP2'
laFileStru[40,2] = 'C'
laFileStru[40,3] = 3
laFileStru[40,4] = 0

laFileStru[41,1] = 'ENTERED'
laFileStru[41,2] = 'C'
laFileStru[41,3] = 10
laFileStru[41,4] = 0

laFileStru[42,1] = 'START'
laFileStru[42,2] = 'C'
laFileStru[42,3] = 10
laFileStru[42,4] = 0

laFileStru[43,1] = 'COMPLETE'
laFileStru[43,2] = 'D'
laFileStru[43,3] = 8
laFileStru[43,4] = 0

laFileStru[44,1] = 'XTERM_DATA'
laFileStru[44,2] = 'C'
laFileStru[44,3] = 30
laFileStru[44,4] = 0


laFileStru[45,1] = 'SZ1'
laFileStru[45,2] = 'C'
laFileStru[45,3] = 5
laFileStru[45,4] = 0

laFileStru[46,1] = 'SZ2'
laFileStru[46,2] = 'C'
laFileStru[46,3] = 5
laFileStru[46,4] = 0

laFileStru[47,1] = 'SZ3'
laFileStru[47,2] = 'C'
laFileStru[47,3] = 5
laFileStru[47,4] = 0

laFileStru[48,1] = 'SZ4'
laFileStru[48,2] = 'C'
laFileStru[48,3] = 5
laFileStru[48,4] = 0

laFileStru[49,1] = 'SZ5'
laFileStru[49,2] = 'C'
laFileStru[49,3] = 5
laFileStru[49,4] = 0

laFileStru[50,1] = 'SZ6'
laFileStru[50,2] = 'C'
laFileStru[50,3] = 5
laFileStru[50,4] = 0

laFileStru[51,1] = 'SZ7'
laFileStru[51,2] = 'C'
laFileStru[51,3] = 5
laFileStru[51,4] = 0

laFileStru[52,1] = 'SZ8'
laFileStru[52,2] = 'C'
laFileStru[52,3] = 5
laFileStru[52,4] = 0


laFileStru[53,1] = 'Note1'
laFileStru[53,2] = 'C'
laFileStru[53,3] = 30
laFileStru[53,4] = 0

laFileStru[54,1] = 'Note2'
laFileStru[54,2] = 'C'
laFileStru[54,3] = 30
laFileStru[54,4] = 0

laFileStru[55,1] = 'XPIECES'
laFileStru[55,2] = 'N'
laFileStru[55,3] = 8
laFileStru[55,4] = 0

laFileStru[56,1] = 'XVALUE'
laFileStru[56,2] = 'N'
laFileStru[56,3] = 8
laFileStru[56,4] = 2

laFileStru[57,1] = 'lhasnotes'
laFileStru[57,2] = 'N'
laFileStru[57,3] = 1
laFileStru[57,4] = 0


=gfCrtTmp(lcTemp,@laFileStru,'XPIKTKT',lcTemp,.F.)





