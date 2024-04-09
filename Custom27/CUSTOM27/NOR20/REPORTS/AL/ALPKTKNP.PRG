*:************************************************************************
*:
*: Procedure file: ALPKTKNP.PRG   FOR CUST #102206
*:                 Custom Pik Ticket for North Port   
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: AHMED MOHAMED EL_ANWAR (AME)
*:      Copyright (c) 
*:  Last modified: 02/13/2001
*:
*:************************************************************************
*:This Program was alo820z in A26 with C#101093
*:************************************************************************ 
*: Modifications
*: B804111,1 HBG 04/04/2001 Moving Company name 10 char right , and the all 
*: B804111,1                report 2 lines up
**************************************************************************** 


STORE SPACE(30) TO lcLongDiv , lcBtName , lcBtAdd1 ,lcBtAdd2 , lcBtAdd3 ,  lcStName , lcStAdd1 ,lcStAdd2 , lcStAdd3
STORE SPACE(15) TO lcCustPo , lcSpecIns
STORE SPACE(10) TO lcAccount , lcApproval
STORE  SPACE(9) TO lcPikTkt        && piktkt
STORE SPACE(7) TO lcStore , lcOrder , lcTermDes , lcShpVaDes
STORE SPACE(6) TO lcDept      && Department
STORE SPACE(5) TO lcFactor    && The factor Code.
STORE {} TO  ldStart , ldComplete
STORE 0 TO lnTotQty,lnHdrRow, lnLinRow, lnBotRow, MaxRow, lnClrLen, lnClrPos, lnStyLen, lnStyPos
STORE .T. TO llFstTim
STORE .F. TO llNoRec
*lcLongDiv      && Division description.
*--Sold to Address
*lcBtName ,lcBtAdd1 ,lcBtAdd2 ,lcBtAdd3 
*--Ship to Address
*lcStName ,lcStAdd1 ,lcStAdd2 ,lcStAdd3 
*lcCustPo   && Cust Po
*lcSpecIns  && Spec. Instr.
*lcAccount  && Account Code.
*lcApproval  && The approval .
*lcStore    && store
*lcOrder    && order No.
*lcTermDes  && Terms Code.
*lcShpVaDes && Sipvia.
*ldStart    && Start Date 
*ldComplete   && Complete Date    
*lnTotQty               && TotQty.  
*lnHdrRow               && No of Row on Headr
*lnLinRow               && No of Row on Body
*lnBotRow               && No of Row on Bottom
*MaxRow                 && Max No Of Rows.
*lnClrLen      && color segment length.
*lnClrPos      && color segment position.  
*lnStyLen      && Major segment length.
*lnStyPos      && Major segment position.  
*llNoRec       && To indicate no records were found
lnInvNo    = 22           && No of Ordline in this page.

DIMENSION laDiv[1,2]    && Array to hold division long name
ladiv[1,1] = 'DIVLNAME'     
ladiv[1,2] = 'lcLongDiv'   &&  variable to hold division long name

*---- Get The Color Segment Position & length
= lfGetMaskD('C',@lnClrLen,@lnClrPos)

*---- Get The Major Segment Position & length
= lfGetMaskD('F',@lnStyLen,@lnStyPos)


SELECT (lcTmpOrdL)
INDEX ON PIKTKT+ORDER+STORE+STYLE+STR(LINENO,6) TAG TmpOrdSt
GO TOP
IF EOF()
  llNoRec = .T.
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT
*-- Main scan loop
SCAN FOR LINENO <> 0
  IF PIKTKT+ORDER+STORE+STYLE+STR(LINENO,6) <>  lcPikTkt + lcOrder +  lcStore 
    IF llFstTim
      llFstTim = .F.
    ELSE
      = lfPrnFootr()     &&--Say Footer    
    ENDIF
    = lfGtInInfo()
    = lfSayHdr()       &&--Say Hdr Data
  ENDIF
  = lfSayLin()       &&--Say Line Data
ENDSCAN

= lfPrnFootr()     &&--Say Footer    


SELECT (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdL)
SET DEVICE TO SCREEN
*---------- END OF Main Proc. Code

****************The functions & Procedures Part****************

*:*************************************************************
*: Program file       : lfGtInInfo
*: Program desc.      : To Load Pick Ticket Data
*: System             : Aria Apparel System.
*: Module             : Custom program number C102206
*: Developer          : AHMED MOHAMED EL_ANWAR (AME)
*: Date               : 02/13/2001
*:*************************************************************
*: Calls              :  lfSolSpAdr() ,
*:*************************************************************
*: Passed Parameters  : Non
*:*************************************************************
*: Example            : =lfGtInInfo()
*:*************************************************************
FUNCTION lfGtInInfo
*call the function lfSolSpAdr from the main prg of the form to get addresses


= lfSolSpAdr()

lcCustPo   = IIF(OrdHdr.MultiPO,Ordline.CustPO,OrdHdr.CUSTPO)
lcDept     = OrdHdr.Dept
lcPikTkt   = PikTkt
lcStore    = Store
lcOrder    = Order
lcSpecIns  = lcSpcInst  && THE Var from main pktkt prog.
lcTermDes  = lcTerms    && THE Var from main pktkt prog.
lcShpVaDes = lcShipVia  && THE Var from main pktkt prog.

lcAccount  = PikTkt.Account
ldStart    = OrdHdr.Start
ldComplete = OrdHdr.Complete

lcFactor   = OrdHdr.CFacCode
lcApproval = OrdHdr.Approval

*-- Sold to Address
lcBtName  = CUSTOMER.BTNAME
lcBtAdd1 = laSoldTo[1]
lcBtAdd2 = laSoldTo[2]
**MH
*ahmed

*lcBtAdd3 = laSoldTo[3]
*lcBtAdd3 = LEFT(laSoldTo[3],ATC('Phone#',laSoldTo[3])-1)
lcBtAdd3 = IIF('Phone#' $ laSoldTo[3],'',laSoldTo[3])
**MH

*-- Ship to Address
lcStName = lcShpTName
lcStAdd1 = laShipTo[1]
lcStAdd2 = laShipTo[2]
**MH
*ahmed 
*lcStAdd3 = laShipTo[3]
*lcStAdd3 = LEFT(laShipTo[3],ATC('Phone#',laShipTo[3])-1)
lcStAdd3 = IIF('Phone#' $ laShipTo[3],'',laShipTo[3])
**MH

*---END OF FUNCTION lfGtInInfo
*:*************************************************************
*: Program file      : lfSayHdr
*: Program desc.     : To Print Hedar
*: System            : Aria Apparel System.
*: Module            : Custom program number C102206
*: Developer          : AHMED MOHAMED EL_ANWAR (AME)
*: Date               : 02/13/2001
*:*************************************************************
*: Calls             : Non
*:*************************************************************
*: Passed Parameters : Non 
*:*************************************************************
*: Example           : =lfSayHdr()
*:*************************************************************
FUNCTION lfSayHdr
IF lnInvNo = 22
  
  *B804111,1 HBG 04/04/2001 moving the all report 2 lines up [Begin]
  *lnHdrRow = 10                   && Start header row
  *lnLinRow = 28                   && Start lines printing row.
  *lnBotRow = 49                   && Start footer row.
  lnHdrRow = 8                   && Start header row
  lnLinRow = 26                   && Start lines printing row.
  lnBotRow = 47                   && Start footer row.
  *B804111,1 [End]
  MaxRow   = lnBotRow - 09         && Max row number for lines printing.
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 11        && Start header row.
  lnLinRow = lnHdrRow + 18        && Start lines printing row.
  lnBotRow = lnLinRow + 21        && Start footer row.
  MaxRow   = lnBotRow - 09        && Max row number for lines printing.
  lnInvNo  = lnInvNo  + 01
ENDIF
=gfRltFld(ORDHDR.CDivision, @laDiv , 'CDIVISION')     && GET DIVISION LONG DESCRIPTION.

lcAdd1 = TRIM(laCompAdd[1]) 
lcAdd2 = TRIM(laCompAdd[2]) 
lcAdd3 = TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5]
IF EMPTY(lcAdd2)
  lcAdd2 = lcAdd3
  lcAdd3 = SPACE(30)
ENDIF
******************************************Header********************************************************************************************
*0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
* /---------------------------division long name---------------------------------/
* /---------------------------Company Address 1 ---------------------------------/
* /---------------------------Company Address 2 ---------------------------------/
* /---------------------------Company Address 3 ---------------------------------/
*     /-----Sold to Name-----------/               /-----Ship to Name-----------/
*     /-----Sold to Address1-------/               /-----Ship to Address1-------/
*     /-----Sold to Address2-------/               /-----Ship to Address2-------/
*     /-----Sold to Address3-------/               /-----Ship to Address3-------/
*
*
*
*
*
*                                            /---Cust Po---/  /Dpt-//store/ /Pktk/
*
*
*
*                                            /Approval/     /-Term Code-/ /Ord#/

* *<///////THE Prev line could be\\\\\\\\\\\>

*                                            /SpcInst-----/ /-Term Code-/ /Ord#/
*
*
*
*   /Acc/     /Ship Via Desc-/ /Start-/    /Cmplet/                   /Shpv/
*0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
*********************************************************************************************************************************************************************
*B804111,1 HBG 04/04/2001 Moving Company name 10 char right [Begin]
*@ lnHdrRow-4,1 SAY PADC(lcLongDiv,80," ")
*@ lnHdrRow-3,1 SAY IIF(llPrntComp,PADC(PADR(lcAdd1,30,' ' ),80," "),'')
*@ lnHdrRow-2,1 SAY IIF(llPrntComp,PADC(PADR(lcAdd2,30,' ' ),80," "),'')
*@ lnHdrRow-1,1 SAY IIF(llPrntComp,PADC(PADR(lcAdd3,30,' ' ),80," "),'')
@ lnHdrRow-5,11 SAY PADC(lcLongDiv,80," ")
@ lnHdrRow-4,1 SAY IIF(llPrntComp,PADC(PADR(lcAdd1,30,' ' ),80," "),'')
@ lnHdrRow-3,1 SAY IIF(llPrntComp,PADC(PADR(lcAdd2,30,' ' ),80," "),'')
@ lnHdrRow-2,1 SAY IIF(llPrntComp,PADC(PADR(lcAdd3,30,' ' ),80," "),'')
*B804111,1[End]
@ lnHdrRow,05 SAY lcBtName
@ lnHdrRow,50 SAY lcStName
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY ALLTRIM(lcBtAdd1)
@ lnHdrRow,50 SAY ALLTRIM(lcStAdd1)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY ALLTRIM(lcBtAdd2)
@ lnHdrRow,50 SAY ALLTRIM(lcStAdd2)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY ALLTRIM(lcBtAdd3)
@ lnHdrRow,50 SAY ALLTRIM(lcStAdd3)
lnHdrRow = lnHdrRow + 5

@ lnHdrRow,44 SAY lcCustPo
@ lnHdrRow,61 SAY lcDept
@ lnHdrRow,67 SAY LEFT(lcStore,7)
@ lnHdrRow,75 SAY lcPikTkt


lnHdrRow = lnHdrRow + 3

@ lnHdrRow,44 SAY IIF((!EMPTY(lcFactor) .AND. !EMPTY(lcApproval)),ALLTRIM(lcApproval),LEFT(lcSpecIns ,14))


@ lnHdrRow,59 SAY LEFT(lcTermDes ,13)
@ lnHdrRow,73 SAY lcOrder
 
lnHdrRow = lnHdrRow + 3

@ lnHdrRow,03 SAY lcAccount
@ lnHdrRow,13 SAY LEFT(lcShpVaDes,16)
@ lnHdrRow,30 SAY ldStart


@ lnHdrRow,42 SAY ldComplete

@ lnHdrRow,69 SAY OrdHdr.Shipvia


*---END OF FUNCTION lfSayHdr

*:*************************************************************
*: Program file      : lfSayLin
*: Program desc.     : To Print Line
*: System            : Aria Apparel System.
*: Module             : Custom program number C102206
*: Developer          : AHMED MOHAMED EL_ANWAR (AME)
*: Date               : 02/13/2001
*:*************************************************************
*: Calls             : Non
*:*************************************************************
*: Passed Parameters : Non 
*:*************************************************************
*: Example           : =lfSayLin()
*:*************************************************************
FUNCTION lfSayLin

******************************************Body********************************************************************************************
*0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
*   /--Style---/ /clr-/       scapk1pk2pk3pk4pk5pk6pk7pk8     totpk  /Price-//Amount/
*   /-Style Desc.-/ /Clr Desc/


*   ***RETURN POLICY*** NO RETURNS ALLOWED AFTER 10 DAYS FROM RECEIPT OF
*   MERCHANDISE. NO RETURNS ACCEPTED WITHOUT AUTHORIZATION LABEL AFFIXED TO
*   PACKAGES. RETURN STICKERS WILL BE ISSUED UPON WRITTEN REQUEST STATING REASON
*   OF RETURN ***
*                      ****** CONTINUED ON NEXT PAGE ******
*0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
*********************************************************************************************************************************************************************

IF lnLinRow > MaxRow 
  @ lnLinRow + 1,03 SAY "***RETURN POLICY*** NO RETURNS ALLOWED AFTER 10 DAYS FROM RECEIPT OF"
  @ lnLinRow + 2,03 SAY "MERCHANDISE. NO RETURNS ACCEPTED WITHOUT AUTHORIZATION LABEL AFFIXED TO"
  @ lnLinRow + 3,03 SAY "PACKAGES. RETURN STICKERS WILL BE ISSUED UPON WRITTEN REQUEST STATING REASON"
  @ lnLinRow + 4,03 SAY "OF RETURN ***"
  @ lnLinRow + 5,00 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ") 
  lnBotRow = lnBotRow+1
  = lfSayHdr()
ENDIF  
@ lnLinRow,03 SAY LEFT(SUBSTR(STYLE,lnStyPos,lnStyLen),12)  
@ lnLinRow,16 SAY LEFT(SUBSTR(STYLE,lnClrPos,lnClrLen),6)  
@ lnLinRow,29 SAY Scale
@ lnLinRow,32 SAY Pik1 		   PICTURE "999"
@ lnLinRow,35 SAY Pik2 		   PICTURE "999"
@ lnLinRow,38 SAY Pik3		   PICTURE "999"
@ lnLinRow,41 SAY Pik4		   PICTURE "999"
@ lnLinRow,44 SAY Pik5 		   PICTURE "999"
@ lnLinRow,47 SAY Pik6  	   PICTURE "999"
@ lnLinRow,50 SAY Pik7         PICTURE "999"
@ lnLinRow,53 SAY Pik8         PICTURE "999"
@ lnLinRow,61 SAY TotPik       PICTURE '99999'
IF llRpStyPrc
  @ lnLinRow,68 SAY Price      PICTURE '99999.99'
ENDIF
@ lnLinRow,76 SAY (Price*TotPik) PICTURE '99999.99'
lnLinRow = lnLinRow + 1  
IF SEEK(Style,"Style")
  @lnLinRow,03 SAY LEFT(Style.Desc,15) 
ENDIF
lcClrDesc = gfCodDes(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR')
IF !EMPTY(lcClrDesc)
  @ lnLinRow,19 SAY LEFT(lcClrDesc,10)
ENDIF
lnLinRow = lnLinRow + 1  


*---END OF FUNCTION lfSayLin
*:*************************************************************
*: Program file       : lfPrnFoot
*: Program desc.      : To print the Pick Ticket Footer
*: System             : Aria Apparel System.
*: Module             : Custom program number C102206
*: Developer          : AHMED MOHAMED EL_ANWAR (AME)
*: Date               : 02/14/2001
*:*************************************************************
*: Calls             : Non
*:*************************************************************
*: Passed Parameters : Non 
*:*************************************************************
*: Example            : =lfPrnFootr()
*:*************************************************************
FUNCTION lfPrnFootr

@ MaxRow +3  ,04 SAY "***RETURN POLICY*** NO RETURNS ALLOWED AFTER 10 DAYS FROM RECEIPT OF"
@ MaxRow +4,04 SAY "MERCHANDISE. NO RETURNS ACCEPTED WITHOUT AUTHORIZATION LABEL AFFIXED TO"
@ MaxRow +5,04 SAY "PACKAGES. RETURN STICKERS WILL BE ISSUED UPON WRITTEN REQUEST STATING REASON"
@ MaxRow +6,04 SAY "OF RETURN ***"

@ lnBotRow,03 SAY  lcAccount
@ lnBotRow,13 SAY  lcOrder
@ lnBotRow,21 SAY  lcPikTkt
@ lnBotRow,30 SAY  lcCustPo
@ lnBotRow,47 SAY  lcDept
@ lnBotRow,53 SAY  lcStore
lnBotRow = lnBotRow + 1
*---END OF FUNCTION lfPrnFootr

*!*************************************************************
*! FUNCTION Name  : lfGetMaskD
*! Developer       :AHMED MOHAMED EL_ANWAR (AME)
*! Date            : 02/14/2001
*! Purpose         : To get segment position & length.
*!*************************************************************
*: Calls              :  gfItemMask()
*:*************************************************************
*! Example            :  =lfGetMaskD()
*!*************************************************************
FUNCTION lfGetMaskD 
PARAMETERS Seg,lnLen,lnPos
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]=Seg
    lnLen = LEN(laItemSeg[lnCount,3])
    lnPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*---END OF FUNCTION lfGetMaskD

*:************************* Program End (Ahmed Mohamed El_anwar)-(AME)