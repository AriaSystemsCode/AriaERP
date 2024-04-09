*****************************************************************************
*: Program file  : MAMATPEI.PRG
*: Program desc. : Fabric Purchase Order form 'EI'.
*:            Custom program  for EILEEN FISHER INC.  (EIL100).
*:         Module: Aria Apparel Series.
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : 
*:         Procedures : lpPrint,lpPart1,lpPart2,lpPart3,lpPart4,lpPart5.
*:         Functions  : lfChkPg(),gfNotePd().
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
lcType = 'EILFMAT'
ll6Lin = .T.
llPrnUsed = gfOpenFile(gcSysHome+'Sycp_Esc' ,'P_Name','SH')
SELECT Sycp_Esc
IF TYPE('_PDPARMS[1]') <> 'U'
  =SEEK(_PDPARMS[1])
  ll6Lin = (ALLTRIM(P_6LPi)=_PDPARMS[7])
ENDIF
USE IN (IIF(llPrnUsed,'SYCP_ESC',0))
lcTerms  = ''
IF FILE(gcDataDir+lcType+'.MEM')
  RESTORE FROM gcDataDir+lcType+'.MEM' ADDITIVE
ENDIF
llVenUsed=gfOpenFile(gcDataDir+'APVENDOR' ,'VenCode','SH')
lcOld = SET("DEVICE")
SET DEVICE TO SCREEN
DO gcRepHome+("MA\MAEIL1.SPX")
SET DEVICE TO &lcOld
HLINE1 = laCompAdd[1]
HLINE5 = ALLTRIM(lcCompPhon)
SET DEVI TO SCREEN
PofHdTmp = gfTempName()
llNoRec  = .F.
lcSelect = ' '
llCompUsd = gfOpenFile(gcSysHome+'SycComp' ,'cComp_Id','SH')
=SEEK(gcAct_Comp,'SycComp')
lcFax = SycComp.cCom_Fax
USE IN (IIF(llCompUsd,'SycComp',0))
DIMENSION laVenAdd[5]
STORE ' ' TO laVenAdd
*-- Print Report.
IF !lfSelPoLn()
  *---Text : 'No Record Selected for the report..!'
   =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  RETURN
ENDIF
DO lpPrint
IF USED(PofHdTmp)
  USE IN (PofHdTmp)
ENDIF
ERASE &gcWorkDir.&PofHdTmp+'.DBF'
RETURN
*****************************************************************************
*: Program file  : lpPrint.PRG
*: Program desc. : Print forms. 
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : DO lpPrint
*****************************************************************************
PROCEDURE lpPrint

lcPortrait  = "CHR(27)+'&l0O'"                  && Portrait Mode
lcNotComp   = "CHR(27)+'&k0S'"                  && 10 cpi
lcComprsd   = "CHR(27)+'&k2S'"                  && 17 cpi
*-for letter paper
IF ll6Lin
  lcPgeSiz  = "CHR(27)+'&l66P'"
  lcLPISpc  = "CHR(27)+'&l6D'"
  lcMaxPage = 59 
ELSE
  lcPgeSiz  = "CHR(27)+'&l88P'"
  lcLPISpc  = "CHR(27)+'&l8D'"
  lcMaxPage = 79 
ENDIF

lcLN1 = 'здддддддддддддддддддддддддбддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддбддддддддддддддддддддддддддддддд©'
lcLN2 = 'Ё WEIGHT:                 Ё CONTENTS:                                                  Ё CANCEL DATES:                 Ё'
lcLN3 = 'Ё WIDTH :                 цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддаддддддддддддддддддддддддддддддд╢'
lcLN4 = 'Ё                         Ё SHRINKAGE NOT              LENGHT:                                                         Ё'
lcLN5 = 'Ё                         Ё TO EXCEED:                 WIDTH :                                                         Ё'
lcLN6 = 'цдддддддддддбдддддддддддддадддбддддддддддддддбддддддддддддддддддддддддддддддддддддддддбддддддддддддддддддбддддддддддддд╢'
lcLN7 = 'Ё STYLE #   Ё    COLOR        Ё    QUANTITY  Ё  DESCRIPTION                           Ё       PRICE/ YD  Ё    AMOUNT   Ё'
lcLN8 = 'цдддддддддддедддддддддддддддддеддддддддддддддеддддддддддддддддддддддддддддддддддддддддеддддддддддддддддддеддддддддддддд╢'
lcLN9 = 'Ё           Ё                 Ё              Ё                                        Ё                  Ё             Ё'
lcLN10= 'цдддддддддддадддддддддддддддддаддддддддддддддаддддддддддддддддддддддддддддддддддддддддаддддддддддддддддддаддддддддддддд╢'
lcLN11= 'Ё APPROX. TOTALS :                                                                                                     Ё'
lcLN12= 'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды'
lcLn13= 'зддддддддддддддддддддддддддддддддддддддбдддддддддддддддддддддддддддддддддддддддбддддддддддддддддддддддддддддддддддддддд©'
lcLn14= 'Ё BOUGHT FROM :                        Ё SHIP TO :                             Ё  DATE :                               Ё'
lcLN15= 'Ё                                      Ё                                       Ё                                       Ё'
lcLN16= 'Ё                                      Ё                                       цддддддддддддддддддддддддддддддддддддддд╢'
lcLN17= 'юддддддддддддддддддддддддддддддддддддддадддддддддддддддддддддддддддддддддддддддаддддддддддддддддддддддддддддддддддддддды'

WAIT WINDOW 'Report printing ! press <SPACE BAR> to abort ' NOWAIT
SET PRINT ON
SET DEVICE TO PRINT

@ 00,00
@ PROW(),PCOL() SAY &lcPortrait+&lcComprsd+&lcPgeSiz+&lcLPISpc

*-- Print loop.
xPAGENO  = 1
lcLnNote = ' '
lnCol    = 00
SELECT &PofHdTmp
SCAN WHILE INKEY()<>32
  =SEEK(&PofHdTmp..cMatType+&PofHdTmp..PoMat,'POFHDR')
  DO lpPart1
  DO lpPart2
  DO lpPart3
  DO lpPart4
  DO lpPart5
ENDSCAN
@ PROW(),PCOL() SAY &lcNotComp
SET PRINT OFF
SET DEVICE TO SCREEN
RETURN

*****************************************************************************
*: Program file  : lpPart1.PRG
*: Program desc. : Print forms. 
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : DO lpPart1
*****************************************************************************
PROCEDURE lpPart1

=SEEK(PofHdr.Vendor,'APVENDOR')
laVenAdd[1] = gfGetAdr('APVENDOR','','','',1)
laVenAdd[2] = gfGetAdr('APVENDOR','','','',2)
laVenAdd[3] = gfGetAdr('APVENDOR','','','',3)
laVenAdd[4] = gfGetAdr('APVENDOR','','','',4)
laVenAdd[5] = gfGetAdr('APVENDOR','','','',5)
lcTermDes = gfCodDes(PofHdr.cTermCode,'CTERMCODE ')
@ 00 ,lnCol SAY '  Page No:'+STR(xPAGENO,3)
@ $+1,lnCol SAY '                                    '+HLINE1
@ $  ,lnCol+95 SAY 'зддддддддддддддддддддддддд©'
@ $+1,lnCol SAY '                                    PHONE:'
@ $  ,lnCol+42 SAY HLINE5  PICTURE gfPhoneTem()  SIZE 1,16
@ $  ,lnCol+95 SAY 'Ё    PURCHASE ORDER #     Ё'
@ $+1,lnCol SAY '                                    '+'FAX  :'+lcFax
@ $  ,lnCol+95 SAY 'Ё        # '+&PofHdTmp..PoMat+'         Ё'
@ $+1,lnCol+95 SAY 'юддддддддддддддддддддддддды'
@ $+1,lnCol SAY ''
@ $+1,lnCol SAY lcLn13
@ $+1,lnCol SAY lcLn14
@ $+1,lnCol SAY lcLn15
@ $  ,lnCol+02 SAY apvendor.cVenComp
@ $  ,lnCol+41 SAY PofHdr.ShpName
@ $  ,lnCol+82 SAY DTOC(PofHdr.Entered)
@ $+1,lnCol    SAY lcLn16
@ $  ,lnCol+02 SAY laVenAdd[1]
@ $  ,lnCol+41 SAY PofHdr.CoutAddr1
@ $+1,lnCol SAY lcLn15

IF !EMPTY(laVenAdd[2])
  @ $,lnCol+2 SAY laVenAdd[2]
ELSE
  @ $,lnCol+2  SAY laVenAdd[3]
*  @ $,PCOL()+1 SAY ALLTRIM(IIF(!EMPTY(laVenAdd[4]),laVenAdd[4],''))
*  @ $,PCOL()+1 SAY ALLTRIM(IIF(!EMPTY(laVenAdd[5]),laVenAdd[5],''))
ENDIF

IF !EMPTY(PofHdr.CoutAddr2)
  @ $,lnCol+41 SAY PofHdr.CoutAddr2
ELSE
  *--- TRIM(SUBSTR(PofHdr.CoutAddr3,1,15)) + ' ' +SUBSTR(PofHdr.cOutAddr4,1,3)+ ' ' +SUBSTR(PofHdr.cOutAddr5,1,10)
  @ $,lnCol+41 SAY TRIM(PofHdr.CoutAddr3)
ENDIF
@ $  ,lnCol+82 SAY 'DEPT :'
@ $+1,lnCol SAY lcLn15

IF !EMPTY(laVenAdd[2])
  @ $,lnCol+2  SAY ALLTRIM(laVenAdd[3])
*  @ $,PCOL()+1 SAY ALLTRIM(IIF(!EMPTY(laVenAdd[4]),laVenAdd[4],''))
*  @ $,PCOL()+1 SAY ALLTRIM(IIF(!EMPTY(laVenAdd[5]),laVenAdd[5],''))
ELSE
  @ $,lnCol+2  SAY ALLTRIM(laVenAdd[4])
ENDIF

IF !EMPTY(PofHdr.CoutAddr2)
  @ $,lnCol+41 SAY TRIM(PofHdr.CoutAddr3)
ELSE
  @ $,lnCol+41 SAY TRIM(PofHdr.CoutAddr4)
ENDIF

lcLnNote = gfNotePd(1)
@ $  ,lnCol+82 SAY lcLnNote
@ $+1,lnCol    SAY lcLn15
@ $  ,lnCol+2  SAY 'ATTN: '+ ApVendor.cVenCont
@ $+1,lnCol    SAY lcLn17
@ $+1,lnCol+2  SAY 'TERMS:'+SUBSTR(lcTermDes,1,15)+'                                 FOB NO:'+PofHdr.cFob

RETURN

*****************************************************************************
*: Program file  : lpPart2.PRG
*: Program desc. : Print forms. 
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : DO lpPart2
*****************************************************************************
PROCEDURE lpPart2

SELECT NotePad
IF SEEK('M'+&PofHdTmp..cMatType+&PofHdTmp..PoMat)
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
  IF lnMTotLin >= 3
    lnMline = 3
    DO WHILE (lnMline <= lnMTotLin) .AND. (lnMline <= 5)
      lcText = MLINE(MNOTES,lnMline)
      IF SUBSTR(ALLTRIM(lcText),1,1) = '*'
        lnMline = lnMline + 1
        LOOP
      ENDIF
      IF PROW()>=lcMaxPage-12
        =lfChkPg(2)
      ENDIF
      @ $+1,lnCol+2 SAY lcText
      lnMline = lnMline + 1
    ENDDO
  ENDIF
  SET MEMOWIDTH TO lnMemWidth
ENDIF
SELECT &PofHdTmp
RETURN

*****************************************************************************
*: Program file  : lpPart3.PRG
*: Program desc. : Print forms. 
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : DO lpPart3
*****************************************************************************
PROCEDURE lpPart3

lcLnNote=gfNotePd(2)
= SEEK(&PofHdTmp..cMatType + &PofHdTmp..PoMat,'POFLN')
= SEEK(PoFLn.Fabric+POFLN.COLOR,'FABRIC')
lcUomUse = FABRIC.UomUse
@ $+1,lnCol     SAY lcLN1
@ $+1,lnCol     SAY lcLN2
@ $  ,lnCol+10  SAY FABRIC.cFabWeight
@ $  ,lnCol+37  SAY SUBSTR(FABRIC.Content,1,50)    && Actual lenth 60
@ $  ,lnCol+103 SAY PofHdr.Complete
@ $+1,lnCol     SAY lcLN3
@ $  ,lnCol+10  SAY FABRIC.Width
@ $+1,lnCol     SAY lcLN4
@ $  ,lnCol+63  SAY ALLTRIM(SUBSTR(ALLTRIM(lcLnNote),1,AT('*',lcLnNote)-1))
@ $+1,lnCol     SAY lcLN5
@ $  ,lnCol+63  SAY ALLTRIM(SUBSTR(ALLTRIM(lcLnNote),AT('*',lcLnNote)+1,50))
@ $+1,lnCol     SAY lcLN6
@ $+1,lnCol     SAY lcLN7
STORE 0 TO lnTotSum,lnTotAmt
SELECT PoFLn
= SEEK(&PofHdTmp..cMatType+&PofHdTmp..PoMat)
SCAN REST WHILE cMatType+PoMat = &PofHdTmp..cMatType+&PofHdTmp..PoMat FOR TRANCD = '1'
  lcClrDes = gfCodDes(PoFLn.Color,'COLOR     ')
  =SEEK(PoFLn.Fabric+PoFLn.Color,'FABRIC')
  @ $+1,lnCol   SAY lcLN8
  @ $+1,lnCol   SAY lcLN9
  @ $,lnCol+02  SAY Fabric
  @ $,lnCol+14  SAY SUBSTR(lcClrDes,1,15)
  @ $,lnCol+32  SAY TotQty  
  @ $,lnCol+47  SAY FABRIC.Desc
  lnPrice = ncost1
  @ $,lnCol+90  SAY '$ '+STR(lnPrice,7,2)+'/'+FABRIC.UomUse
  @ $,lnCol+107 SAY (TotQty * lnPrice ) PICTURE '999999.99'
  lnTotSum = lnTotSum + TotQty  
  lnTotAmt = lnTotAmt + (TotQty * lnPrice )
  IF PROW() >= lcMaxPage-15
    =lfChkPg(3)
  ENDIF
ENDSCAN

llPrnt = .F.
llFrst = .T.
SELECT NotePad
IF SEEK('M'+&PofHdTmp..cMatType+&PofHdTmp..PoMat)
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
  lnMline   = 6
  IF lnMTotLin >= 6
    DO WHILE lnMline <= lnMTotLin
      lcText  = MLINE(MNOTES,lnMline)
      IF SUBSTR(ALLTRIM(lcText),1,1) <> '*'
        @ $+1,lnCol SAY lcLN8
        @ $+1,lnCol SAY lcLN9
        @ $  ,lnCol+47 SAY SUBSTR(lcTEXT,1,38)
      ENDIF
      lnMline = lnMline + 1
      IF PROW()>=lcMaxPage-15
        =lfChkPg(3)
      ENDIF
    ENDDO
  ENDIF
  SET MEMOWIDTH TO lnMemWidth
ENDIF
SELECT &PofHdTmp
@ $+1,lnCol    SAY lcLN10
@ $+1,lnCol    SAY lcLN11
@ $  ,lnCol+31 SAY lnTotSum PICTURE '99999999.999'
@ $  ,lnCol+44 SAY lcUomUse
@ $  ,lnCol+105 SAY "$" + ALLTRIM(STR(lnTotAmt,11,2))
@ $+1,lnCol     SAY lcLN12
RETURN

*****************************************************************************
*: Program file  : lpPart4.PRG
*: Program desc. : Print forms. 
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*: EXAMPLE : DO lpPart4
*****************************************************************************
PROCEDURE lpPart4

lcMemoWdth=SET('MEMOWIDTH')
SET MEMOWIDTH TO 76
FOR I=1 TO MEMLINES(lcTerms)
  IF PROW()>=lcMaxPage-12
    =lfChkPg(4)  
  ENDIF
  @ $+1,lnCol+2 SAY MLINE(lcTerms,I)
ENDFOR
SET MEMOWIDTH TO (lcMemoWdth)
RETURN

*****************************************************************************
*: Program file  : lpPart5.PRG
*: Program desc. : Print forms. 
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : DO lpPart5
*****************************************************************************
PROCEDURE lpPart5

@ lcMaxPage-10,lnCol+11 SAY 'EILEEN FISHER, INC.                    ACCEPTED BY:'
@ lcMaxPage-8,lnCol+15 SAY 'BY________________________________                    ________________________________'
@ lcMaxPage-7,lnCol+15 SAY '     NAME/TITLE                                           NAME/TITLE '
@ lcMaxPage-5,lnCol+17 SAY '________________________________                    ________________________________ ' 
@ lcMaxPage-4,lnCol+17 SAY '       DATE                                               COMPANY'
@ lcMaxPage-2,lnCol+17 SAY '                                                    ________________________________ ' 
@ lcMaxPage-1,lnCol+17 SAY '                                                           DATE'
@ lcMaxPage,lnCol SAY ''
SELECT &PofHdTmp
xPAGENO = xPAGENO + 1
RETURN

*****************************************************************************
*: Program file  : lfChkPg.PRG
*: Program desc. : FABRIC PURCHASE ORDER print.
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : =lfChkPg()
*****************************************************************************
FUNCTION lfChkPg
PARAMETER lnPrtNO

IF lnPrtNO = 3
  @ $+1,lnCol SAY lcLN10
  @ $+1,lnCol SAY lcLN11
  @ $+1,lnCol SAY lcLN12
  *--Start New Page.
  xPAGENO = xPAGENO + 1
  @ 01 ,lnCol SAY '  Page No:'+STR(xPAGENO,3) 
  @ $+1,lnCol SAY '                                                                                               зддддддддддддддддддддддддд©'  
  @ $+1,lnCol SAY '                                                                                               Ё    PURCHASE ORDER #     Ё'
  @ $+1,lnCol SAY '                                                                                               Ё        # '+&PofHdTmp..PoMat+'         Ё'
  @ $+1,lnCol SAY '                                                                                               юддддддддддддддддддддддддды'
  @ $+1,lnCol SAY ''
  =SEEK(PoFLn.Fabric+POFLN.COLOR,'FABRIC')
  @ $+1,lnCol     SAY lcLN1
  @ $+1,lnCol     SAY lcLN2
  @ $  ,lnCol+10  SAY FABRIC.cFabWeight
  @ $  ,lnCol+37  SAY SUBSTR(FABRIC.Content,1,50)
  @ $  ,lnCol+103 SAY PofHdr.Complete
  @ $+1,lnCol     SAY lcLN3
  @ $  ,lnCol+10  SAY FABRIC.Width
  @ $+1,lnCol     SAY lcLN4
  @ $  ,lnCol+63  SAY ALLTRIM(SUBSTR(ALLTRIM(lcLnNote),1,AT('*',lcLnNote)-1))
  @ $+1,lnCol     SAY lcLN5
  @ $  ,lnCol+63  SAY ALLTRIM(SUBSTR(ALLTRIM(lcLnNote),AT('*',lcLnNote)+1,50))
  @ $+1,lnCol     SAY lcLN6
  @ $+1,lnCol     SAY lcLN7
ELSE
  xPAGENO = xPAGENO + 1
  @ 00,lnCol SAY '  Page No:'+STR(xPAGENO,3) 
  @ $+1,lnCol SAY '                                                                                               зддддддддддддддддддддддддд©'  
  @ $+1,lnCol SAY '                                                                                               Ё    PURCHASE ORDER #     Ё'
  @ $+1,lnCol SAY '                                                                                               Ё        # '+&PofHdTmp..PoMat+'         Ё'
  @ $+1,lnCol SAY '                                                                                               юддддддддддддддддддддддддды'
  @ $+1,lnCol SAY ''
ENDIF
RETURN

*****************************************************************************
*: Program file  : gfNotePd.PRG
*: Program desc. : Note Pad.
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : =gfNotePd()
*****************************************************************************
FUNCTION gfNotePd
PARAMETER lnNteln

lcLnNote = ' '
SELECT NotePad
IF SEEK( 'M'+&PofHdTmp..cMatType+&PofHdTmp..PoMat )
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  lcText = MLINE(MNOTES,lnNteln)
  IF SUBSTR(ALLTRIM(lcText),1,1) <> '*'  
    lcLnNote =  SUBSTR(lcText,1,25)
  ENDIF
  SET MEMOWIDTH TO lnMemWidth
ENDIF
SELECT &PofHdTmp
RETURN(lcLnNote)

*****************************************************************************
*: Program file  : lfSelPoLn.PRG
*: Program desc. : Select Po's Lines.
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : =lfSelPoLn()
*****************************************************************************
FUNCTION lfSelPoLn

SELECT POFHDR
LOCATE FOR &lcRpExp
IF FOUND()
  COPY REST TO (gcWorkDir+PofHdTmp) FOR &lcRpExp
  SELECT 0
  USE &gcWorkDir.&PofHdTmp
ELSE
  RETURN (.F.)
ENDIF

*****************************************************************************
*: Program file  : lfVMsgSave.PRG
*: Program desc. : Valid save button.
*:      Developer: Ahmed Salah Shalaby -(SSH)
*:           Date: 03/17/99m
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : =lfVMsgSave()
*****************************************************************************
FUNCTION lfVMsgSave

SET MEMOWIDTH TO 78
IF MEMLINES(lcTerms) > 20
  *---Text "Number of rows can not be more than 20 lines. If you proceed, only the first 20 lines will be printed."
  IF gfModalGen('QRM36134B36007','DIALOG') = 1
    SAVE TO gcDataDir+lcType+'.MEM' ALL LIKE lcTerms
    CLEAR READ
  ENDIF
ELSE
  SAVE TO gcDataDir+lcType+'.MEM' ALL LIKE lcTerms
  CLEAR READ
ENDIF