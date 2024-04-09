****************************************************************************
*: Program file : ALPKTKZ.PRG    
*: DESC : PRINT PICKING TICKETS (For YL)
*: System: Aria2.7 
*: Module: Sales order allocation
*: DATE : 09/01/99
*: Developer: Sherif Attala Ishak 
*C101578,1 Refer to.
*:************************************************************************
*: Calls : 
*:************************************************************************
*: Modifications :
****************************************************************************

*-- Initializing Variable.
lcCustPo   = SPACE(15)
lcDept     = SPACE(5)
lcStore    = SPACE(8)
lcOrder    = SPACE(6)
lcSpecIns  = SPACE(30)
lcTermDes  = SPACE(30)
lcAccount  = SPACE(5)
lcShpVaDes = SPACE(30)
lcDiv      = SPACE(30)
lcSViaCod  = SPACE(5)
lcPikTkt   = SPACE(9)
ldStart    = {}
lcLongDiv  = SPACE(30)
ldComplete = {}             
ldDate     = {}  
lcNote1    = SPACE(30)
lcNote2    = SPACE(30) 

DIMENSION laScales(4,2)

lcFactor  = SPACE(5)          && The factor number.
lcApproval  = SPACE(10)       && The approval number.

ldCancel   = {}
lnInvNo    = 22
lnTotQty   = 0
lnHdrRow   = 0
lnLinRow   = 0
lnBotRow   = 0
MaxRow     = 0

CLEAR TYPEAHEAD
XTIME = TIME()
SET DEVICE TO PRINT

*- Ship to Address
lcBtName   = SPACE(30)
lcBtAdd1   = SPACE(30)
lcBtAdd2   = SPACE(30)
lcBtAdd3   = SPACE(30)

lcBtName   = SPACE(30)
lcBtAdd1   = SPACE(30)
lcBtAdd2   = SPACE(30)
lcBtAdd3   = SPACE(30)
lcFBtName   = SPACE(30)
lcFBtAdd1   = SPACE(30)
lcFBtAdd2   = SPACE(30)
lcFBtAdd3   = SPACE(30)

*--Sold to Address
lcStName   = SPACE(30)
lcStAdd1   = SPACE(30)
lcStAdd2   = SPACE(30)
lcStAdd3   = SPACE(30)
*-- Main loop.
SELECT &LCTMPORDL
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

DO WHILE !EOF(LCTMPORDL)
 lcPikTkt = PikTkt
 lnTotAmt = 0
 IF SEEK(lcPikTkt,"PikTkt")
   =lfGtInInfo()
   SELECT &LCTMPORDL
   = lfSay()  
 ENDIF
 SELECT PikTkt
 REPLACE PRTFLAG WITH 'P'
ENDDO
SET DEVICE TO SCREEN
*:*************************************************************
*: Program file       : lfGtInInfo
*: Program desc.      : To Load Pick Ticket Data
*: System             : Aria Apparel System.
*: Module             : Custom program number C101093
*: Developer          : Ahmed Salah Shalaby - (SSH)
*:*************************************************************
*: Calls              : lfCodDescr()
*:*************************************************************
*: Passed Parameters  : Non
*:*************************************************************
*: Example            : =lfGtInInfo()
*:*************************************************************
FUNCTION lfGtInInfo
lcCustPo   = IIF(OrdHdr.MultiPO,Ordline.CustPO,OrdHdr.CUSTPO)
lcDept     = OrdHdr.Dept
lcStore    = PikTkt.Store
lcOrder    = Order
lcSpecIns  = gfCodDes(ORDHDR->SPCINST , 'SPCINST')
lcTermDes  = gfCodDes(ORDHDR->CTERMCODE , 'CTERMCODE')
lcAccount  = PikTkt.Account
lcShpVaDes = gfCodDes(ORDHDR->SHIPVIA , 'SHIPVIA')
lcDiv      = gfCodDes(ORDHDR->cDivision ,'cDivision')
ldStart    = OrdHdr.Start
ldComplete = OrdHdr.Complete
lcFactor   = OrdHdr.cFacCode
lcApproval = OrdHdr.Approval
lcNote1    = ORDHDR.Note1 
lcNote2    = ordhdr.Note2 
 
ldCancel   = OrdHdr.Cancelled
lcCusExp   = IIF(EMPTY(lcStore),"M","S") + lcAccount +;
             IIF(EMPTY(lcStore),"",lcStore)
ldDate     = PIKTKT.Date

STORE SPACE(0) TO laScales

IF SEEK(lcCusExp,"Customer")
  lcBtName = Customer.BtName
  lcBtAdd1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  lcBtAdd2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
  lcBtAdd3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2') 
  IF EMPTY(lcBtAdd2)
    lcBtAdd2 = lcBtAdd3
    lcBtAdd3 = ''
  ENDIF
ENDIF

*-- Sold to Address
SELECT ORDHDR
IF Alt_ShpTo
  lcStName = OrdHdr.STName   
  lcStAdd1 = OrdHdr.cAddress1
  lcStAdd2 = OrdHdr.cAddress2
  lcStAdd3 = OrdHdr.cAddress3
  IF LEN(TRIM(lcStAdd2)) =0
    lcStAdd2 = lcStAdd3
    lcStAdd3 = ''
  ENDIF
ELSE
  SELECT CUSTOMER
  IF SEEK(lcCusExp,"Customer")
    IF !EMPTY(Dist_Ctr) 
      lcDistCntr = ALLTRIM(Dist_Ctr)
      =seek('S' + ACCOUNT + lcDistCntr)
    ENDIF
    lcStName  = IIF( EMPTY(DBA) , STNAME , DBA)
    lcStAdd1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    lcStAdd2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    lcStAdd3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
  ENDIF
ENDIF

IF EMPTY(lcStAdd2)
   lcStAdd2 = lcStAdd3
   lcStAdd3 = SPACE(30)
ENDIF

*:*************************************************************
*: Program file       : lfSay
*: Program desc.      : To print  All Pick Ticket {Hdr,Line,Footer}
*: System             : Aria Apparel System.
*: Module             : Custom program number C101093
*: Developer          : Ahmed Salah Shalaby - (SSH)
*:*************************************************************
*: Calls : FUNCTIONS  :  lfSayHdr()      && Say Hdr Data
*:                    :  lfSayLin()      && Say Line Data
*:                    :  lfPrnFootr()    && Say Footer
*:*************************************************************
*: Passed Parameters  : NON
*:*************************************************************
*: Returns            : Null
*:*************************************************************
*: Example            : =lfSay()
*:*************************************************************
FUNCTION lfSay
= lfSayHdr()       &&--Say Hdr Data
= lfSayLin()       &&--Say Line Data
= lfPrnFootr()     &&--Say Footer

*:*************************************************************
*: Program file      : lfSayHdr
*: Program desc.     : To Print Hedar
*: System            : Aria Apparel System.
*: Module            : Custom program number C101093
*: Developer         : Ahmed Salah Shalaby - (SSH)
*:*************************************************************
*: Calls             : Non
*:*************************************************************
*: Passed Parameters : Non 
*:*************************************************************
*: Example           : =lfSayHdr()
*:*************************************************************
*: Modifications:
*: B801502,1 ADEL 02/25/98 Prevent the program from printing a line 
*: B801502,1 ADEL          up (in case of multi pick tickets) starting 
*: B801502,1 ADEL          from the 3 rd page.
*:*************************************************************
FUNCTION lfSayHdr
IF lnInvNo = 22
  lnHdrRow = 5                    && Start header row
  lnLinRow = 23                   && Start lines printing row.
  lnBotRow = 43                   && Start footer row.
  MaxRow   = lnBotRow - 09         && Max row number for lines printing.
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 12        && Start header row.
  lnLinRow = lnHdrRow + 18        && Start lines printing row.
  lnBotRow = lnLinRow + 20        && Start footer row.
  MaxRow   = lnBotRow - 09        && Max row number for lines printing.
  lnInvNo  = lnInvNo  + 01
ENDIF

@ lnHdrRow - 5,73 SAY lcPikTkt
@ lnHdrRow - 3,73 SAY ldDate

@ lnHdrRow - 1,8 SAY ORDHDR.Account
@ lnHdrRow - 1,50 SAY lcStore
@ lnHdrRow - 1,70 SAY "ORDER : " + ORDHDR. Order


@ lnHdrRow,08 SAY lcBtName
@ lnHdrRow,50 SAY lcStName

lnHdrRow = lnHdrRow + 1
@ lnHdrRow,08 SAY ALLTRIM(lcBtAdd1)
@ lnHdrRow,50 SAY ALLTRIM(lcStAdd1)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,08 SAY ALLTRIM(lcBtAdd2)
@ lnHdrRow,50 SAY ALLTRIM(lcStAdd2)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,08 SAY ALLTRIM(lcBtAdd3)
@ lnHdrRow,50 SAY ALLTRIM(lcStAdd3)


lnHdrRow = lnHdrRow + 4
@ lnHdrRow,64 SAY IIF(ORDHDR.Status = "C","/","")
lnHdrRow = lnHdrRow + 1

@ lnHdrRow,1 SAY lcCustPo
@ lnHdrRow,16 SAY lcDept
@ lnHdrRow,24 SAY IIF((!EMPTY(lcFactor) .AND. !EMPTY(lcApproval)),ALLTRIM(lcApproval),"")
@ lnHdrRow,37 SAY ORDHDR.Rep1
@ lnHdrRow,46 SAY LEFT(lcTermDes ,15)
@ lnHdrRow,64 SAY IIF(ORDHDR.Status <> "C","/","")
*@ lnHdrRow,67 SAY LEFT(lcStore,7)
*@ lnHdrRow,75 SAY lcPikTkt

lnHdrRow = lnHdrRow + 3

@ lnHdrRow,1 SAY ldStart
@ lnHdrRow,12 SAY ORDHDR.Complete
@ lnHdrRow,22 SAY LEFT(lcShpVaDes,20)
@ lnHdrRow,42 SAY LEFT(lcDiv,9)
 
lnOldRec = RECNO()
lnCount = 1
SCAN WHILE PIKTKT = lcPikTkt AND lnCount <= 4
  IF ASCAN(laScales,SCALE.Scale) = 0 
    laScales(lnCount,1) = SCALE.Scale
    laScales(lnCount,2) = PADL(ALLTRIM(SCALE.SZ1),4)+;
             PADL(ALLTRIM(SCALE.SZ2),4)+PADL(ALLTRIM(SCALE.SZ3),4) +;
             PADL(ALLTRIM(SCALE.SZ4),4)+PADL(ALLTRIM(SCALE.SZ5),4) +;
             PADL(ALLTRIM(SCALE.SZ6),4)+PADL(ALLTRIM(SCALE.SZ7),4) +;
             PADL(ALLTRIM(SCALE.SZ8),4)

    lnCount = lnCount + 1
  ENDIF
ENDSCAN
GOTO lnOldRec

FOR lnRCnt = 1 TO 4
  @ lnHdrRow + lnRCnt + 1,33 SAY LEFT(laScales(lnRCnt,1),1) + laScales(lnRCnt,2)
ENDFOR


*:*************************************************************
*: Program file      : lfSayLin
*: Program desc.     : To Print Line
*: System            : Aria Apparel System.
*: Module            : Custom program number C101093
*: Developer         : Ahmed Salah Shalaby - (SSH)
*:*************************************************************
*: Calls             : Non
*:*************************************************************
*: Passed Parameters : Non 
*:*************************************************************
*: Example           : =lfSayLin()
*:*************************************************************
*: MODIFICATION:
*: B801502,1 ADEL 02/25/98 Prevent the program from printing a line 
*: B801502,1 ADEL          up (in case of multi pick tickets) starting 
*: B801502,1 ADEL          from the 3 rd page.
*:*************************************************************
FUNCTION lfSayLin
SCAN WHILE PIKTKT = lcPikTkt
  IF lnLinRow > MaxRow 
    lnBotRow = lnBotRow+1
    = lfSayHdr()
  ENDIF  
  @ lnLinRow,1 SAY LEFT(STYLE,8)  
  @ lnLinRow,8 SAY  SUBSTR(STYLE,ATC("-",style)+1,4) 
  @ lnLinRow,13 SAY LEFT(STYLE.Desc,19)
  @ lnLinRow,33 SAY LEFT(SCALE,1)
  @ lnLinRow,34 SAY IIF(!EMPTY(PIK1),STR(Pik1,4),"")
  @ lnLinRow,38 SAY IIF(!EMPTY(PIK2),STR(Pik2,4),"")
  @ lnLinRow,42 SAY IIF(!EMPTY(PIK3),STR(Pik3,4),"")
  @ lnLinRow,46 SAY IIF(!EMPTY(PIK4),STR(Pik4,4),"")
  @ lnLinRow,50 SAY IIF(!EMPTY(PIK5),STR(Pik5,4),"")
  @ lnLinRow,54 SAY IIF(!EMPTY(PIK6),STR(Pik6,4),"")
  @ lnLinRow,58 SAY IIF(!EMPTY(PIK7),STR(Pik7,4),"")
  @ lnLinRow,62 SAY IIF(!EMPTY(PIK8),STR(Pik8,4),"")
  @ lnLinRow,74 SAY IIF(!EMPTY(TOTPIK),STR(TOTPIK,6),"") 
  @ lnLinRow,80 SAY IIF(!EMPTY(PRICE),STR(PRICE,7,2),"")  
  lnLinRow = lnLinRow + 1
ENDSCAN
lnLinRow = MaxRow  
*:*************************************************************
*: Program file       : lfPrnFoot
*: Program desc.      : To print the Pick Ticket Footer
*: System    		  : Aria Apparel System.
*: Module             : Custom program number C101093
*: Developer          : Ahmed Salah Shalaby - (SSH)
*:*************************************************************
*: Calls              : Null
*:*************************************************************
*: Passed Parameters  : Null
*:*************************************************************
*: Returns            : Null
*:*************************************************************
*: Example            : =lfPrnFootr()
*:*************************************************************
FUNCTION lfPrnFootr

@ lnBotRow + 2,3 SAY lcNote1
@ lnBotRow + 3,3 SAY lcNote2
lnBotRow = lnBotRow + 1
