*!********************************************************************
*: Program file  : ALPKTKWE.PRG
*: Program desc. : CUSTOMIZED P/T FORMAT FOR WEARWOLF GROUP LTD
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 4XP
*:         Module: ALLOCATION (AL)(C200737)T20070117.0014 - C200737
*:      Developer: Mariam Mazhar[MMT]
*!********************************************************************
SELECT (lcTmpOrdL)
LOCATE 
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

loOgScroll.cCRorientation = 'P'
loOgScroll.lcLogoPath = ''

lcTempPik =loogscroll.gfTempName()
SELECT (lcTmpOrdL)
DIMENSION laFileStruct[1,18]
lnFileFld = AFIELDS(laFileStruct)
DIMENSION laFileStruct[lnFileFld+20,18]

laFileStruct[lnFileFld+1 ,1] = 'cShipViaDes'
laFileStruct[lnFileFld+1 ,2] = 'C'
laFileStruct[lnFileFld+1 ,3] = 30
laFileStruct[lnFileFld+1 ,4] = ''

laFileStruct[lnFileFld+2 ,1] = 'StName'
laFileStruct[lnFileFld+2 ,2] = 'C'
laFileStruct[lnFileFld+2 ,3] = 30
laFileStruct[lnFileFld+2 ,4] = ''

laFileStruct[lnFileFld+3 ,1] = 'CAdres1'
laFileStruct[lnFileFld+3 ,2] = 'C'
laFileStruct[lnFileFld+3 ,3] = 30
laFileStruct[lnFileFld+3 ,4] = ''

laFileStruct[lnFileFld+4 ,1] = 'CAdres2'
laFileStruct[lnFileFld+4 ,2] = 'C'
laFileStruct[lnFileFld+4 ,3] = 30
laFileStruct[lnFileFld+4 ,4] = ''

laFileStruct[lnFileFld+5 ,1] = 'CAdres3'
laFileStruct[lnFileFld+5 ,2] = 'C'
laFileStruct[lnFileFld+5 ,3] = 100
laFileStruct[lnFileFld+5 ,4] = ''


laFileStruct[lnFileFld+6 ,1] = 'DEPTART'
laFileStruct[lnFileFld+6 ,2] = 'C'
laFileStruct[lnFileFld+6 ,3] = 5
laFileStruct[lnFileFld+6 ,4] = ''


laFileStruct[lnFileFld+7 ,1] = 'Ord_Start'
laFileStruct[lnFileFld+7,2] = 'D'
laFileStruct[lnFileFld+7 ,3] = 8
laFileStruct[lnFileFld+7 ,4] = ''



laFileStruct[lnFileFld+8 ,1] = 'Ord_Comp'
laFileStruct[lnFileFld+8,2] = 'D'
laFileStruct[lnFileFld+8 ,3] = 8
laFileStruct[lnFileFld+8 ,4] = ''


laFileStruct[lnFileFld+9 ,1] = 'Clr_DESC'
laFileStruct[lnFileFld+9,2] = 'C'
laFileStruct[lnFileFld+9 ,3] = 30
laFileStruct[lnFileFld+9 ,4] = ''


laFileStruct[lnFileFld+10 ,1] = 'cscl_desc'
laFileStruct[lnFileFld+10,2] = 'C'
laFileStruct[lnFileFld+10 ,3] = 10
laFileStruct[lnFileFld+10 ,4] = ''

laFileStruct[lnFileFld+11 ,1] = 'SZ_1'
laFileStruct[lnFileFld+11,2] = 'C'
laFileStruct[lnFileFld+11 ,3] = 5
laFileStruct[lnFileFld+11 ,4] = ''


laFileStruct[lnFileFld+12 ,1] = 'SZ_2'
laFileStruct[lnFileFld+12,2] = 'C'
laFileStruct[lnFileFld+12 ,3] = 5
laFileStruct[lnFileFld+12 ,4] = ''

laFileStruct[lnFileFld+13 ,1] = 'SZ_3'
laFileStruct[lnFileFld+13,2] = 'C'
laFileStruct[lnFileFld+13 ,3] = 5
laFileStruct[lnFileFld+13 ,4] = ''

laFileStruct[lnFileFld+14 ,1] = 'SZ_4'
laFileStruct[lnFileFld+14,2] = 'C'
laFileStruct[lnFileFld+14 ,3] = 5
laFileStruct[lnFileFld+14 ,4] = ''

laFileStruct[lnFileFld+15 ,1] = 'SZ_5'
laFileStruct[lnFileFld+15,2] = 'C'
laFileStruct[lnFileFld+15 ,3] = 5
laFileStruct[lnFileFld+15 ,4] = ''

laFileStruct[lnFileFld+16 ,1] = 'SZ_6'
laFileStruct[lnFileFld+16,2] = 'C'
laFileStruct[lnFileFld+16 ,3] = 5
laFileStruct[lnFileFld+16 ,4] = ''

laFileStruct[lnFileFld+17 ,1] = 'SZ_7'
laFileStruct[lnFileFld+17,2] = 'C'
laFileStruct[lnFileFld+17 ,3] = 5
laFileStruct[lnFileFld+17 ,4] = ''

laFileStruct[lnFileFld+18 ,1] = 'SZ_8'
laFileStruct[lnFileFld+18,2] = 'C'
laFileStruct[lnFileFld+18 ,3] = 5
laFileStruct[lnFileFld+18 ,4] = ''

laFileStruct[lnFileFld+19 ,1] = 'o_note1'
laFileStruct[lnFileFld+19,2] = 'C'
laFileStruct[lnFileFld+19 ,3] = 30
laFileStruct[lnFileFld+19 ,4] = ''

laFileStruct[lnFileFld+20 ,1] = 'o_note2'
laFileStruct[lnFileFld+20,2] = 'C'
laFileStruct[lnFileFld+20 ,3] = 30
laFileStruct[lnFileFld+20 ,4] = ''



FOR LOOP =1 TO 20
  STORE ' ' TO  laFileStruct[lnFileFld+Loop,7],laFileStruct[lnFileFld+Loop,8],;
                laFileStruct[lnFileFld+Loop,9],laFileStruct[lnFileFld+Loop,10],;
                laFileStruct[lnFileFld+Loop,11],laFileStruct[lnFileFld+Loop,12],;
                laFileStruct[lnFileFld+Loop,13],laFileStruct[lnFileFld+Loop,14],;
                laFileStruct[lnFileFld+Loop,15],laFileStruct[lnFileFld+Loop,16]
  STORE 0 TO    laFileStruct[lnFileFld+Loop,17] ,laFileStruct[lnFileFld+Loop,18]
ENDFOR   



= gfCrtTmp(lcTempPik ,@laFileStruct,'PIKTKT',lcTempPik ,.T.)

lcPrnComp  = IIF(llPrntComp,'Y','N')
lcNonMajTl = ''
lcNonMajPi = ''
STORE 0 TO lnFreeLen , lnColorLen , lnMajSeg,lnNonMajSt

lnMajLen = LEN(gfItemMask('PM'))
=lfGetColor()

SELECT (lcTmpOrdL)
SCAN FOR !EMPTY(Style)
  WAIT WINDOW 'PRINT PICK TICKETS......' NOWAIT
  SCATTER MEMO MEMVAR 
  =SEEK('O'+m.order,lcOrdHdr)
  m.Store    = &lcPiktktTemp..Store
  m.Account  = &lcPiktktTemp..Account
  m.CustPO   = &lcPiktktTemp..CustPo
  m.Deptart     = &lcOrdHdr..Dept
  m.Ord_Start= &lcOrdHdr..Start
  m.Ord_Comp = &lcOrdHdr..Complete
  m.cscl_desc = &lcScaleFile..cscl_desc
  m.SZ_1 	 = &lcScaleFile..SZ1
  m.SZ_2 	= &lcScaleFile..SZ2
  m.SZ_3 = &lcScaleFile..SZ3
  m.SZ_4 = &lcScaleFile..SZ4
  m.SZ_5 = &lcScaleFile..SZ5
  m.SZ_6 = &lcScaleFile..SZ6
  m.SZ_7 = &lcScaleFile..SZ7
  m.SZ_8 = &lcScaleFile..SZ8


  IF llRpOrdNot AND loNotePad.SEEK('B'+m.order)
    m.note_mem = &lcTempNotePad..mnotes
  ENDIF   

  IF llRpOrdNot 
    m.o_note1 = &lcOrdHdr..note1
    m.o_note2 = &lcOrdHdr..note2
  ENDIF 


  IF SEEK('S'+m.Account+m.Style,lcPiktktTemp)
    m.Pack_id  = SUBSTR(&lcPiktktTemp..Pack_ID,1,8)
  ENDIF
  lcColor = SUBSTR(&lcTmpOrdL..Style,lnNonMajSt,lnColorLen)
  m.Clr_DESC = SUBSTR(gfCodDes(lcColor,'COLOR     '),1,7)
  
  IF &lcOrdHdr..Alt_ShpTo
    m.StName  = &lcOrdHdr..StName
    m.CAdres1 = &lcOrdHdr..cAddress1
    m.CAdres2 = &lcOrdHdr..cAddress2
    m.CAdres3 = ALLTRIM(&lcOrdHdr..cAddress3) + ' ' +ALLTRIM(&lcOrdHdr..cAddress4) +;
      ' ' + ALLTRIM(&lcOrdHdr..cAddress5)
    IF EMPTY(m.CAdres2)
      m.CAdres2  = m.CAdres3
      m.CAdres3 = ''
    ENDIF 
  ELSE
    =SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store) ,lcCustomer)
    DIMENSION laAddress[6,3]
	  *-- ShipTo address
	  =gfGetAdr(lcCustomer,'','','',1,'')
	  m.StName  = &lcCustomer..StName
	  m.CAdres1 = laAddress[1,2]
	  m.CAdres2 = laAddress[2,2]
	  m.CAdres3 = TRIM(laAddress[3,2]) + ' ' +TRIM(laAddress[4,2]) +;
	  ' ' + ALLTRIM(laAddress[5,2])
	  IF EMPTY(m.CAdres2)
	    m.CAdres2 = m.CAdres3 
	    m.CAdres3 = ''
	  ENDIF
    m.cShipViaDes = ALLTRIM(gfCodDes(&lcOrdHdr..ShipVia,'SHIPVIA   '))
  
  ENDIF 
  INSERT INTO (lcTempPik ) FROM MEMVAR 
ENDSCAN 



SELECT (lcTempPik)


IF RECCOUNT(lcTempPik) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF



lcTempPrnFl = loogscroll.gfTempName()
COPY TO oAriaApplication.WorkDir +  lcTempPrnFl  + ".DBF"

DIMENSION loOGScroll.laCRParams[3,2]

loOGScroll.laCRParams[1,1] = 'lnColorLen'
loOGScroll.laCRParams[1,2] = lnColorLen

loOGScroll.laCRParams[2,1] = 'lnNonMajSt'
loOGScroll.laCRParams[2,2] = lnNonMajSt

loOGScroll.laCRParams[3,1] = 'llRpOrdNot'
loOGScroll.laCRParams[3,2] = IIF(llRpOrdNot,1,0)

*MMT
STORE 0 TO lnTotPik
STORE .F. TO llNotePrntd
*MMT
DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempPrnFl  + ".DBF"
gfDispRe()
RETURN 

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Mariam Mazhar[MMT]
*! Date      : 01/25/2007
*! Purpose   : To get the length to of the color.
*!*************************************************************
*! Calls       : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

lcNonMajTl = ''
lcNonMajPi = ''
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
 *-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen



*!*************************************************************
*! Name      : lfCountRec
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/03/2007
*! Purpose   : To get the no of records per piktkt
*!*************************************************************
FUNCTION lfCountRec
PARAMETERS lcPikTK
STORE 0 TO lnCounting
lcOldAlis = SELECT()
SELECT (lcTempPik)
lnOldRec = RECNO()
COUNT FOR Piktkt = lcPikTK TO lnCounting
GO RECORD lnOldRec 
SELECT(lcOldAlis) 
RETURN lnCounting

*!*************************************************************
*! Name      : lfIncRec
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/03/2007
*! Purpose   : To increment the record count
*!*************************************************************
FUNCTION lfIncRec
lnRecordin = lnRecordin + 1
lnTotPik = lnTotPik + totpik
RETURN lnRecordin 

*!*************************************************************
*! Name      : lfResetVar
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/03/2007
*! Purpose   : To reset the record count
*!*************************************************************
FUNCTION lfResetVar
IF lnRecordin  = lnCountRec
  lnTotPik = 0
ENDIF 
llNotePrntd = .F.
RETURN .T.

*!*************************************************************
*! Name      : lfNotePrint
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/03/2007
*! Purpose   : To check if notes printed or not
*!*************************************************************
FUNCTION lfNotePrint
IF llRpOrdNot AND !EMPTY(note_mem)
  llNotePrntd = .T.
ELSE
  llNotePrntd = .F.
ENDIF   
RETURN .T.