*:***************************************************************************
*: Program file  : ARPINVWW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR Warewolf.
*: Date          : 11/02/2007
*: System        : Aria4XP
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Ahmed Salah
*: Tracking Job Number: C200745 - T20070118.0034
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVWW
*:***************************************************************************
*:Modifications :
*:***************************************************************************
*!*	_screen.Visible = .T.
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND 


IF llOgFltCh
llDonprnt=.F.
SELECT INVHDR
SET RELATION OFF INTO ORDHDR
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO INVLINE

SELECT INVLINE
SET RELATION OFF INTO SPCK_LIN
SET RELATION OFF INTO STYLE

SELECT STYLE
SET RELATION OFF INTO SCALE


** create expession begin
lcSeek=' .T. '

*-- INVOICE Filter
lcInvFltr= lfCheckFilter(3, 'INVHDR.INVOICE')
llInvFltr   = !EMPTY(lcInvFltr) AND USED(lcInvFltr) AND RECCOUNT(lcInvFltr) > 0
IF llInvFltr   
  SELECT (lcInvFltr)
  INDEX ON INVOICE  TAG (lcInvFltr)
  lcSeek=lcSeek+" AND SEEK(INVOICE,'"+lcInvFltr+"')"
ELSE
  IF TYPE("lcInvFltr") = "C" AND USED(lcInvFltr)
    USE IN (lcInvFltr)
  ENDIF
  lcInvFltr= ''
ENDIF


*-- CUSTOMER Filter
lcAccFltr= lfCheckFilter(3, 'INVHDR.ACCOUNT')
llAccFltr   = !EMPTY(lcAccFltr) AND USED(lcAccFltr) AND RECCOUNT(lcAccFltr) > 0
IF llAccFltr   
  SELECT (lcAccFltr)
  INDEX ON ACCOUNT   TAG (lcAccFltr)
  lcSeek=lcSeek+" AND SEEK(ACCOUNT,'"+lcAccFltr+"')"
ELSE
  IF TYPE("lcAccFltr") = "C" AND USED(lcAccFltr)
    USE IN (lcAccFltr)
  ENDIF
  llAccFltr   = ''
ENDIF

*Date Filter

lnDatePos  = ASUBSCRIPT(laOGVRFlt,ASCAN(laOGVRFlt,'INVHDR.INVDATE'),1)
LDATE = SUBSTR(laOGVRFlt[lnDatePos,6],1,ATC('|',laOGVRFlt[lnDatePos,6])-1)
HDATE = SUBSTR(laOGVRFlt[lnDatePos,6],  ATC('|',laOGVRFlt[lnDatePos,6])+1)

IF !EMPTY(LDATE)
  lcSeek=lcSeek +" AND  BETWEEN(INVDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
ELSE
  IF  !EMPTY(HDATE)
    lcSeek=lcSeek +" AND  INVDATE<=CTOD('"+HDATE+"') "
  ENDIF
ENDIF 


lcSeek=lcSeek+" AND "+IIF(lcFactrInv = 'F' , "!EMPTY(INVHDR.cFacCode) AND INVHDR.STATUS<>'V' " ,;
                   IIF(lcFactrInv = 'N' , "EMPTY(INVHDR.cFacCode) AND INVHDR.STATUS<>'V' " , "INVHDR.STATUS<>'V'"))

IF !EMPTY( LCRPPRST)
  lcSeek=lcSeek+IIF(LCRPPRST='P'," AND INVHDR.PRTFLAG='P' "," AND INVHDR.PRTFLAG<>'P' ")
ENDIF
** create expression end

PRIVATE laInvScal , laScalStr , Stylen

DIMENSION laInvScal[1,10] , laScalStr[5]
STORE '' TO laInvScal , laScalStr

STORE 0 TO lnLenth , lnClrLen , lnClrPos
*--THE STYLE LENGTH
lnLenth = LEN(gfItemMask('PM'))
*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

STORE SPACE(0) TO Stylen
Stylen = lnLenth + lnClrLen + 1          && 1 FOR "-"
lcTempInv = loOgScroll.gfTempName()
InvScal= loOgScroll.gfTempName()

=lfBuildTmp()  


*--Section collect the data.
SELECT INVLINE
SET RELATION TO STYLE INTO STYLE
SELECT INVHDR
SCAN FOR &lcSeek  
  WAIT WINDOW 'Selecting Records For The Invoice ...' + INVHDR.Invoice NOWAIT
  =lfCollData()
  =lfFilInv()
ENDSCAN
SELECT INVLINE
SET RELATION OFF INTO STYLE 

SELECT (lcTempInv )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF  
SET ORDER TO INVLINES 


=lfAdjustCRSettings()

IF USED(lcTempInv )
    USE IN (lcTempInv )
ENDIF

IF USED(INVSCAL )
    USE IN (INVSCAL )
ENDIF
IF TYPE("loogscroll.lcLogoPath") = 'C' .AND. !EMPTY(loogscroll.lcLogoPath) 
  loogscroll.lcLogoPath=''
ENDIF
=gfDispRe()
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  &&FILTER CHANGE


*!*************************************************************
*! Name      : gfGetZone
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =gfGetZone()
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk

IF !USED('FRTZONES')
  lnOldWrk = SELECT()
  SELECT 0
  DO NETUSE WITH '&QLB.FRTZONES','&QLB.FRTZONES','SH'
  SELECT (lnOldWrk)
ENDIF

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'FRTZONES'),'',FRTZONES.ZONE)

*-- End of gfGetZone.


*--End of lfGetScale.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Collect the invline data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias , lcRcNoHdr , lcoldOrd,lntotalAmt,lnsumQty

STORE SPACE(0) TO lcAlias , lcRcNoHdr , lcoldOrd
lcAlias = ALIAS()
lcRcNoHdr = EVAL(KEY())
lntotalAmt = 0
lnsumQty=0
SELECT (lcTempInv)
lcoldOrd = ORDER()
SET ORDER TO TAG lcTempInv

SELECT INVLINE
=SEEK(INVHDR.INVOICE)
SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
  SCATTER MEMVAR MEMO  
  lntotalAmt = lntotalAmt + (Price*TotQty)
  lnsumQty = lnsumQty+TotQty
  SELECT (lcTempInv)
  =SEEK("S" + INVLINE.SCALE , 'SCALE')
  SELECT (lcTempInv)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE &lcTempInv..DESC WITH ALLTRIM(STYLE.DESC)
  REPLACE LCOLOR   WITH SUBSTR(STYLE,lnClrPos,4),;
          ColrDesc WITH ALLTRIM(gfCodDes(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR')),;
          scldesc  WITH ALLTRIM(SCALE.cscl_desc),;
          nAmount  WITH nAmount+lntotalAmt,;
          SumQty   WITH SumQty+lnsumQty
ENDSCAN
SELECT (lcTempInv)
SET ORDER TO TAG &lcoldOrd

=SEEK(lcRcNoHdr)
SELECT(lcAlias)

*--End of lfCollData.



*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]
DIMENSION loOgScroll.laCRParams[11,2]


loOgScroll.lcOGLastForm ='ARPINVWW'
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempInv+ ".DBF"
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  INVSCAL + ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= ''


STORE "" TO HLINE1,HLINE2,HLINE3,HLINE4,HLINE5

loOgScroll.laCRParams[2,1] = 'HLINE1'
loOgScroll.laCRParams[2,2] = HLINE1 

loOgScroll.laCRParams[3,1] = 'HLINE2'
loOgScroll.laCRParams[3,2] = HLINE2 

loOgScroll.laCRParams[4,1] = 'HLINE3'
loOgScroll.laCRParams[4,2] = HLINE3 

loOgScroll.laCRParams[5,1] = 'HLINE4'
loOgScroll.laCRParams[5,2] = HLINE4 

loOgScroll.laCRParams[6,1] = 'HLINE5'
loOgScroll.laCRParams[6,2] = HLINE5 

loOgScroll.laCRParams[7,1] = 'LCPRNOTE'
loOgScroll.laCRParams[7,2] = IIF(LLRPINVNOT ,'Y','N')

loOgScroll.laCRParams[8,1] = 'METHOD'
loOgScroll.laCRParams[8,2] = 'M'

loOgScroll.laCRParams[9,1] = 'lnLenth'
loOgScroll.laCRParams[9,2] = lnLenth 

loOgScroll.laCRParams[10,1] = 'lnClrLen'
loOgScroll.laCRParams[10,2] = lnClrLen 

loOgScroll.laCRParams[11,1] = 'llPrntComp'
loOgScroll.laCRParams[11,2] = 0


*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

DIMENSION laTempStru[22,18] ,laTempLINE[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempLINE
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT INVLINE
=OGAFIELDS(@laTempLINE)
laTempStru[1,1]  = 'STYLE'
laTempStru[2,1]  = 'INVOICE'
laTempStru[3,1]  = 'LINENO'
laTempStru[4,1]  = 'QTY1'
laTempStru[5,1]  = 'QTY2'
laTempStru[6,1]  = 'QTY3'
laTempStru[7,1]  = 'QTY4'
laTempStru[8,1]  = 'QTY5'
laTempStru[9,1]  = 'QTY6'
laTempStru[10,1]  = 'QTY7'
laTempStru[11,1]  = 'QTY8'
laTempStru[12,1]  = 'TOTQTY'
laTempStru[13,1]  = 'PRICE'
laTempStru[14,1]  = 'SCALE'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 14
  lnFldRow = ASCAN(laTempLINE,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempLINE,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempLINE[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempLINE[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempLINE[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

laTempStru[15,1] = 'DESC'
laTempStru[15,2] = 'C'
laTempStru[15,3] = 14
laTempStru[15,4] = 0

laTempStru[16,1] = 'QTY9'
laTempStru[16,2] = 'N'
laTempStru[16,3] = 7
laTempStru[16,4] = 0

*!*	FOR lnCrtTmp = 1 TO 9
*!*	  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
*!*	  laTempStru[lnCrtTmp +16,1] = 'SZ'+lcNumSiz 
*!*	  laTempStru[lnCrtTmp +16,2] = 'C'
*!*	  laTempStru[lnCrtTmp +16,3] = 5
*!*	  laTempStru[lnCrtTmp +16,4] = 0
*!*	ENDFOR
laTempStru[17,1] = 'ORDER'
laTempStru[17,2] = 'C'
laTempStru[17,3] = 6
laTempStru[17,4] = 0

laTempStru[18,1] = 'LCOLOR'
laTempStru[18,2] = 'C'
laTempStru[18,3] = 10
laTempStru[18,4] = 0

laTempStru[19,1] = 'ColrDesc'
laTempStru[19,2] = 'C'
laTempStru[19,3] = 7
laTempStru[19,4] = 0

laTempStru[20,1] = 'scldesc'
laTempStru[20,2] = 'C'
laTempStru[20,3] = 10
laTempStru[20,4] = 0

laTempStru[21,1] = 'nAmount'
laTempStru[21,2] = 'N'
laTempStru[21,3] = 14
laTempStru[21,4] = 3


laTempStru[22,1] = 'SumQty'
laTempStru[22,2] = 'N'
laTempStru[22,3] = 7
laTempStru[22,4] = 0

=gfCrtTmp(lcTempInv,@laTempstru,,"",.f.)

SELECT (lcTempInv)
INDEX ON LEFT(STYLE,Stylen) + INVOICE TAG lcTempInv OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE
INDEX ON ORDER + STR(LINENO,6) + INVOICE TAG INVLINEO OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE
INDEX ON STYLE + INVOICE + STR(LINENO,6) TAG INVLINES OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINES OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE

DIMENSION laTempStru[109,18] ,laTempHDR[1,18]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempHDR
SELECT INVHDR
=OGAFIELDS(@laTempHDR)
laTempStru[1,1]  = 'INVOICE'
laTempStru[2,1]  = 'INVDATE'
laTempStru[3,1]  = 'CUSTPO'
laTempStru[4,1]  = 'PHONE'
laTempStru[5,1]  = 'NOTE1'
laTempStru[6,1]  = 'NOTE2'
laTempStru[7,1]  = 'ORDER'
laTempStru[8,1]  = 'PIKTKT'
laTempStru[9,1]  = 'ACCOUNT'
laTempStru[10,1]  = 'SEASON'
laTempStru[11,1]  = 'CDIVISION'
laTempStru[12,1]  = 'STORE'
laTempStru[13,1]  = 'REP1'
laTempStru[14,1]  = 'REP2'
laTempStru[15,1]  = 'SHIPDATE'
laTempStru[16,1]  = 'CARTONS'
laTempStru[17,1]  = 'WEIGHT'
laTempStru[18,1]  = 'TOTALCHG'
laTempStru[19,1]  = 'DISCOUNT'
laTempStru[20,1]  = 'TAX_RATE'
laTempStru[21,1]  = 'TAX_AMT'
laTempStru[22,1]  = 'NPSTRATE'
laTempStru[23,1]  = 'NPSTAMT'
laTempStru[24,1]  = 'NHSTRATE'
laTempStru[25,1]  = 'NHSTAMT'
laTempStru[26,1]  = 'APPROVAL'
laTempStru[27,1]  = 'DEPT'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 27
  lnFldRow = ASCAN(laTempHDR,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempHDR,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempHDR[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempHDR[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempHDR[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)


laTempStru[28,1] = 'BTNAME'
laTempStru[28,2] = 'C'
laTempStru[28,3] = 50
laTempStru[28,4] = 0

laTempStru[29,1] = 'BTADDR1'
laTempStru[29,2] = 'C'
laTempStru[29,3] = 50
laTempStru[29,4] = 0

laTempStru[30,1] = 'BTADDR2'
laTempStru[30,2] = 'C'
laTempStru[30,3] = 50
laTempStru[30,4] = 0


laTempStru[31,1] = 'BTADDR3'
laTempStru[31,2] = 'C'
laTempStru[31,3] = 50
laTempStru[31,4] = 0

laTempStru[32,1] = 'STNAME'
laTempStru[32,2] = 'C'
laTempStru[32,3] = 50
laTempStru[32,4] = 0

laTempStru[33,1] = 'STADDR1'
laTempStru[33,2] = 'C'
laTempStru[33,3] = 50
laTempStru[33,4] = 0

laTempStru[34,1] = 'STADDR2'
laTempStru[34,2] = 'C'
laTempStru[34,3] = 50
laTempStru[34,4] = 0

laTempStru[35,1] = 'STADDR3'
laTempStru[35,2] = 'C'
laTempStru[35,3] = 50
laTempStru[35,4] = 0

laTempStru[36,1] = 'TERMS'
laTempStru[36,2] = 'C'
laTempStru[36,3] = 50
laTempStru[36,4] = 0

laTempStru[37,1] = 'SHIPVIA'
laTempStru[37,2] = 'C'
laTempStru[37,3] = 50
laTempStru[37,4] = 0

laTempStru[38,1] = 'SPCINST'
laTempStru[38,2] = 'C'
laTempStru[38,3] = 50
laTempStru[38,4] = 0

laTempStru[39,1] = 'WKAMT'
laTempStru[39,2] = 'N'
laTempStru[39,3] = 10
laTempStru[39,4] = 2

laTempStru[40,1] = 'MSG2'
laTempStru[40,2] = 'C'
laTempStru[40,3] = 70
laTempStru[40,4] = 0

laTempStru[41,1] = 'MSG1'
laTempStru[41,2] = 'C'
laTempStru[41,3] = 70
laTempStru[41,4] = 0

laTempStru[42,1] = 'MNOTES'
laTempStru[42,2] = 'M'
laTempStru[42,3] = 10
laTempStru[42,4] = 0


FOR I= 1 TO 6
  laTempStru[I+42,1] = 'SC'+allTRIM(STR(I))
  laTempStru[I+42,2] = 'C'
  laTempStru[I+42,3] = 3
  laTempStru[I+42,4] = 0
ENDFOR
lnInd=0
FOR I=1 TO 6
  FOR J=1 TO 9
    lnInd=lnInd+1
    laTempStru[lnInd+48,1] = 'SC'+allTRIM(STR(I))+"_"+allTRIM(STR(j))
    laTempStru[lnInd+48,2] = 'C'
    laTempStru[lnInd+48,3] = 5
    laTempStru[lnInd+48,4] = 0
  ENDFOR
ENDFOR  



laTempStru[103,1] = 'FNAME'
laTempStru[103,2] = 'C'
laTempStru[103,3] = 40
laTempStru[103,4] = 0

laTempStru[104,1] = 'FADDR1'
laTempStru[104,2] = 'C'
laTempStru[104,3] = 40
laTempStru[104,4] = 0

laTempStru[105,1] = 'FADDR2'
laTempStru[105,2] = 'C'
laTempStru[105,3] = 40
laTempStru[105,4] = 0

laTempStru[106,1] = 'FADDR3'
laTempStru[106,2] = 'C'
laTempStru[106,3] = 40
laTempStru[106,4] = 0

laTempStru[107,1] = 'lnotes'
laTempStru[107,2] = 'C'
laTempStru[107,3] = 1
laTempStru[107,4] = 0

laTempStru[108,1] = 'TAX_DESC'
laTempStru[108,2] = 'C'
laTempStru[108,3] = 50
laTempStru[108,4] = 0


laTempStru[109,1] = 'TAX_Refr'
laTempStru[109,2] = 'C'
laTempStru[109,3] = 50
laTempStru[109,4] = 0


=gfCrtTmp(InvScal,@laTempstru,,"",.f.)
SET EXACT &lcExcStat 

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString



*!*************************************************************
*! Name      : lfFilInv
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 08/11/2006
*! Purpose   : Filling file InvScale
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfFilInv

lcScalStr = SPACE(0)
SELECT INVHDR
SCATTER MEMVAR MEMO 
SELECT (InvScal)
APPEND BLANK
GATHER MEMVAR MEMO

REPLACE  PHONE   WITH TRANSFORM(InvHdr.PHONE ,'@R '+ lcPhonPict),;
         NOTE1   WITH IIF(InvHdr.NOTE1<>'*' AND llRpInvLWW , InvHdr.NOTE1, 'D'),;
         NOTE2 	 WITH IIF(InvHdr.NOTE2<>'*' AND llRpInvLWW, InvHdr.NOTE2, 'D'),;
         TERMS   WITH gfCodDes(INVHDR.CTERMCODE,'CTERMCODE'),;
         SHIPVIA WITH gfCodDes(INVHDR.SHIPVIA,'SHIPVIA'),;
         SPCINST WITH gfCodDes(INVHDR.SPCINST,'SPCINST')


XSTORE=Store
SELECT CUSTOMER
SEEK IIF(EMPTY(ALLTRIM(INVHDR.STORE)),'M'+INVHDR.ACCOUNT,'S'+INVHDR.ACCOUNT+XSTORE)
=lfSolSpAdr()
SELECT (InvScal)
REPLACE  BTNAME		WITH   lcSolTName
REPLACE  BTADDR1 	WITH   laSoldTo[1]
REPLACE  BTADDR2 	WITH   laSoldTo[2]
REPLACE  BTADDR3	WITH   TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF LEN(TRIM(laSoldTo[2])) =0
  REPLACE  BTADDR2  WITH   laSoldTo[3]
  REPLACE  BTADDR3	WITH   ''
ENDIF
REPLACE  STNAME		WITH   lcShpTName
REPLACE  STADDR1 	WITH   laShipTo[1]
REPLACE  STADDR2 	WITH   laShipTo[2]
REPLACE  STADDR3	WITH   TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
IF LEN(TRIM(laSoldTo[2])) =0
  REPLACE  STADDR2	WITH   laShipTo[3]
  REPLACE  STADDR3	WITH   ''
ENDIF

lcInvNo   = InvHDR.Invoice

*!*	XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
*REPLACE  TAX_DESC		WITH   XTAX_DESC

*!*	IF !EMPTY(lcTaxRefr)
*!*	  REPLACE  TAX_Refr	WITH   lcTaxRefr
*!*	ENDIF  
REPLACE  TAX_RATE	WITH   INVHDR.TAX_RATE
REPLACE  TAX_AMT	WITH   INVHDR.TAX_AMT
IF InvHdr.nPSTAmt <> 0
  REPLACE  nPSTRate	WITH   InvHdr.nPSTRate
  REPLACE  nPSTAmt	WITH   InvHdr.nPSTAmt
ENDIF  
IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry ))='CANADA' AND InvHdr.nHSTAmt <> 0 
  REPLACE  nHSTRate		WITH   InvHdr.nHSTRate
  REPLACE  nHSTAmt 		WITH   InvHdr.nHSTAmt
ENDIF  

WKMT = InvHdr.FREIGHT &&+ InvHdr.INSUR + InvHdr.COD
IF WKMT <> 0
 REPLACE  WKAMT 		WITH   WKMT
ENDIF

IF INVHDR.APPROVAL<>' ' .AND. UPPER(INVHDR.APPROVAL)<>'DEC'
  REPLACE  APPROVAL	WITH   INVHDR.APPROVAL
ENDIF
**   'TOTAL - D I S C O U N T'
REPLACE  DISCOUNT	WITH   INVHDR.DISCOUNT

IF LLPRNFACT AND !(EMPTY(INVHDR.cFacCode))
  XFNAME  = lcFacName             && Factor company name.
  XFADDR1 = laFactor[1]           && Factor address#1.
  XFADDR2 = laFactor[2]
  XFADDR3 = laFactor[3]
  IF LEN(TRIM(laFactor[2])) = 0  && If address#2 is empty fill it with address#3.
    XFADDR2 = laFactor[3]
    XFADDR3 = ''
  ENDIF
SELECT (INVSCAL)
	REPLACE FNAME WITH XFNAME
	REPLACE FADDR1 WITH XFADDR1
	REPLACE FADDR2 WITH XFADDR2
	REPLACE FADDR3 WITH XFADDR3

ENDIF


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

