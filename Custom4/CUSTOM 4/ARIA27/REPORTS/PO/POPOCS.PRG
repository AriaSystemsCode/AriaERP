*:************************************************************************
*: Program file  : POPOCS.Prg
*: Program desc. : Style Purchase orders Cost Sheet Report.
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 08/23/1998
*: REF           : *E300977,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetTit(), lfwOGWhen(), lfCalculate(), lfsrvTrans(), 
*:                 lfvVend(), lfwOldVal()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO POPOCS
*:************************************************************************
*: Modifications:
*B802043,1 AMM 03/21/99 1-Fix the bug of not printing "Used QTY"&"Act Ext Cost"
*B802043,1 AMM          2- Display PO's of status "C" in the PO's Browse.
*B603348,1 ABD 02/07/2000 Increase The Picture Of The purchase price per item [UnTCost]
*B603348,1                To 9999999.999 & the Picture Of The TotCost [ ToTCost ] 
*B603348,1                To 999999999999.99 Instead of the fields.
*B603713,10 HBG 17/12/2000 Fix bug of **** appears in price
*B604465,1  MHM 09/05/2001 Fix bug of The total cost line is printed on a separate page  
*B606012,1  AMH 06/04/2002 Fix bug of print extd cost with different currencies
*B606645,1  KHM 11/14/2002 Fix bug of variable lcNoToPrn not found in case of PO cost sheet
*B606645,1                 start with Mfg operation then Fabric/any cost element.
*B606622,1  AMH 11/14/2002 Fix bug of not printing the  actual po.
*B127487,1  EIH 05/05/2005 Fix bug printing the actual po with out canceled status .
*:************************************************************************
qTopic='Print Style P/O Cost Sheet'

*-- Get the cost elements titles
DIMENSION laCost[10,2]
laCost[1,1]  = 'M_CITYPE1 '
laCost[2,1]  = 'M_CITYPE2 '
laCost[3,1]  = 'M_CITYPE3 '
laCost[4,1]  = 'M_CITYPE4 '
laCost[5,1]  = 'M_CITYPE5 '
laCost[6,1]  = 'M_CISLBL1 '
laCost[7,1]  = 'M_CISLBL2 '
laCost[8,1]  = 'M_CISLBL3 '
laCost[9,1]  = 'M_CISLBL4 '
laCost[10,1] = 'M_CISLBL5 '
=gfGetMemvar(@laCost,gcAct_Comp)

FOR lnI = 1 TO 5
  lcI = ALLTRIM(STR(lnI))
  lcShrt&lcI = laCost[lnI+5,2]
  lcLong&lcI = laCost[lnI+5,2]
ENDFOR
*-- Get memory variables
*-- Use detailed costing for imported styles
llImpCost = gfGetMemvar('M_LIMPCOST',gcAct_Comp)
*-- Dyelot YES/NO
llDyelot = (gfGetMemvar('M_DYELOT',gcAct_Comp)  ='Y')
*-- Style markup Top/Bottom
llStyMrk = (gfGetMemvar('M_STYMARK',gcAct_Comp) = 'T')

IF !llImpCost
  * Message  'System has not been setup to use detailed costing
  *           therefore P/O cost sheet report is not available.'
  =gfModalGen('TRM34123B00000','DIALOG')
  RETURN
ENDIF

PoTemp = gfTempName()

* Add the filter of status

*B606622,1  AMH Change the variable name from lcStatus to lcRpStatus [Start]
*IF lcStatus <> "L"
*  lcRpExp = '(' +lcRpExp+ ") .AND. NSTYORDER <> 0 .AND. (POSHDR.Status = '" + lcStatus +  "')"
*B802043,1 AMM Add the status to the filter.
*ELSE
*  lcRpExp = '(' +lcRpExp+ ") .AND. NSTYORDER <> 0 .AND. POSHDR.Status $ 'COHS' "
*B802043,1 AMM end
*ENDIF
IF lcRpStatus <> "L"
  lcRpExp = '(' +lcRpExp+ ") .AND. NSTYORDER <> 0 .AND. (POSHDR.Status = '" + lcRpStatus +  "')"
ELSE
  lcRpExp = '(' +lcRpExp+ ") .AND. NSTYORDER <> 0"
  *B127487,1  EIH 05/05/2005 Fix bug printing the actual po with out canceled status [Begin]
  lcRpExp = lcRpExp +  ".AND. POSHDR.Status <> 'X' "
  *B127487,1  EIH 05/05/2005 [END]
ENDIF
*B606622,1  AMH [End]

SELE POSHDR
GO TOP

WAIT WINDOW 'Selecting records for report ...' NOWAIT
LOCATE ALL FOR &lcRpExp

IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF


SET TALK ON
COPY REST TO (gcWorkDir+PoTemp) FOR &lcRpExp
SET TALK OFF

=gfOpenFile(gcWorkDir+PoTemp,'','EX')

Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'SORTING &Z RECORDS FOR P/O COST SHEET REPORT ...' NOWAIT
INDEX ON PO TAG &PoTemp
GO TOP

R_TITLE  = 'STYLE P/O COST SHEET REPORT'
R_WIDTH  = 'W'
XREPORT  = 'POPOCS'
XTIME    = TIME()
PAGENO   = 0
ROW      = 99

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

SELECT &PoTemp
GO TOP
*--REPORT LOOP.
DO WHILE .T.
  IF EOF()
    EXIT
  ENDIF

  IF ROW >=51
    PAGENO = PAGENO+1
    
    *B606622,1  AMH Change variable name from xtitle to lcrptitle [Start]
    *DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    DO RPT_HDR WITH XREPORT,lcRpTITLE,R_WIDTH
    *B606622,1  AMH [End]
    
    @ 05,00 SAY ' PO#    STATUS COMPLETE   AVAILABLE  VENDOR   VENDOR NAME                   DIVISION              WORK IN PROCESS ACCOUNT   WHOUSE'
    @ 06,00 SAY REPLICATE('=',132)
    ROW = 07
  ENDIF

  IF ROW >=50
    ROW = 99
    LOOP
  ENDIF

  *-- Get the vendor name
  lcVenName = IIF(SEEK(&PoTemp..Vendor,'APVENDOR'),APVENDOR.cVenComp,'')

  lcDivision = gfCodDes(&PoTemp..CDivision, 'CDIVISION')
  SELECT GL_Link
  lcWIPAcnt = &PoTemp->Link_Code + '-'+IIF(SEEK(&PoTemp->Link_Code+'013'),SUBSTR(GLACNT,1,18),'')

  SELECT &PoTemp
  *--LINE 1.
  @ ROW,001 SAY Po
  DO CASE
    CASE STATUS = 'O'
      lcStat='OPEN'
    CASE STATUS = 'C'
      lcStat='CMPLTE'
    CASE STATUS = 'X'
      lcStat='CANCEL'
    CASE STATUS = 'S'
      lcStat='CLOSE'
    
    *B606622,1  AMH Print the actual po [Start]
    CASE STATUS = 'A'
      lcStat='ACTUAL'
    *B606622,1  AMH [End]
    
    OTHE
      lcStat='HOLD'
  ENDCASE
  @ ROW,008 SAY lcStat
  @ ROW,015 SAY Complete
  @ ROW,026 SAY Available
  @ ROW,037 SAY Vendor
  @ ROW,046 SAY LEFT(lcVenName,28)
  @ ROW,076 SAY CDivision+' '+LEFT(lcDivision,14)
  @ ROW,098 SAY lcWIPAcnt
  @ ROW,124 SAY IIF(EMPTY(cWareCode),'Multi WH',cWareCode)
  ROW = ROW + 2
  *B603348,1 ABD Remark The next Line & Move Them To The Left & Right to Indicate The Layout. [Begin]
  *@ ROW,017 SAY 'ESTIMATED     LANDED     ACTUAL   ACT-LAND   LAND-EST    ACT-EST   ACT/LAND   LAND/EST    ACT/EST'
  *B603713,10 HBG 01/22/2001 Increase the picture of cost items (AMH)[Begin]
  *@ ROW,015 SAY 'ESTIMATED     LANDED     ACTUAL   ACT-LAND       LAND-EST        ACT-EST  ACT/LAND  LAND/EST   ACT/EST'
  @ ROW,015 SAY 'ESTIMATED        LANDED        ACTUAL   ACT-LAND   LAND-EST    ACT-EST  ACT/LAND  LAND/EST   ACT/EST'
  *B603713,10 (AMH)[End]
  *B603348,1 ABD [ End ]
  ROW = ROW + 1
  *B603348,1 ABD Remark the next lines & move them to the left & change the picture 
  *B603348,1     Instead of the fields. [Begin]
  *@ ROW,003 SAY lcShrt1
  *@ ROW,016 SAY nICost1 PICTURE '999999.999'
  *@ ROW,027 SAY nLan_Cost1 PICTURE '999999.999'
  *@ ROW,038 SAY nAct_Cost1 PICTURE '999999.999'
  *@ ROW,049 SAY nAct_Cost1 - nLan_Cost1 PICTURE '999999.999'
  *@ ROW,060 SAY nLan_Cost1 - nICost1 PICTURE '999999.999'
  *@ ROW,071 SAY nAct_Cost1 - nICost1 PICTURE '999999.999'
  *@ ROW,082 SAY IIF(nLan_Cost1=0,'   N/A',(nAct_Cost1 / nLan_Cost1)*100) PICTURE '999999.999'
  *@ ROW,093 SAY IIF(nICost1=0,'   N/A',(nLan_Cost1 / nICost1)*100) PICTURE '999999.999'              
  *@ ROW,104 SAY IIF(nICost1=0,'   N/A',(nAct_Cost1 / nICost1)*100) PICTURE '999999.999'              
  *@ ROW,116 SAY 'ORDER  :'
  *@ ROW,125 SAY NSTYOrder   PICTURE '9999999'
  
  @ ROW,001 SAY lcShrt1
  @ ROW,011 SAY nICost1 PICTURE    '999999999.999'
  *B603713,10 HBG 01/22/2001 Increase the picture of cost items (AMH)[Begin]
  *@ ROW,025 SAY nLan_Cost1 PICTURE '999999.999'
  *@ ROW,036 SAY nAct_Cost1 PICTURE '999999.999'
  *@ ROW,047 SAY nAct_Cost1 - nLan_Cost1 PICTURE '999999.999'
  *@ ROW,058 SAY nLan_Cost1 - nICost1 PICTURE '9999999999.999'
  *@ ROW,073 SAY nAct_Cost1 - nICost1 PICTURE '9999999999.999'
  *@ ROW,088 SAY IIF(nLan_Cost1=0,' N/A',(nAct_Cost1 / nLan_Cost1)*100) PICTURE '99999.999'
  *@ ROW,098 SAY IIF(nICost1=0,' N/A',(nLan_Cost1 / nICost1)*100) PICTURE '99999.999'              
  *@ ROW,108 SAY IIF(nICost1=0,' N/A',(nAct_Cost1 / nICost1)*100) PICTURE '99999.999'              
  *@ ROW,118 SAY 'ORDER  :'
  @ ROW,025 SAY nLan_Cost1 PICTURE '999999999.999'
  @ ROW,039 SAY nAct_Cost1 PICTURE '999999999.999'
  @ ROW,053 SAY nAct_Cost1 - nLan_Cost1 PICTURE '999999.999'
  @ ROW,064 SAY nLan_Cost1 - nICost1 PICTURE '999999.999'
  @ ROW,075 SAY nAct_Cost1 - nICost1 PICTURE '999999.999'
  @ ROW,086 SAY IIF(nLan_Cost1=0,' N/A',(nAct_Cost1 / nLan_Cost1)*100) PICTURE '99999.999'
  @ ROW,096 SAY IIF(nICost1=0,' N/A',(nLan_Cost1 / nICost1)*100) PICTURE '99999.999'              
  @ ROW,106 SAY IIF(nICost1=0,' N/A',(nAct_Cost1 / nICost1)*100) PICTURE '99999.999'              
  @ ROW,116 SAY 'ORDER  :'
  *B603713,10 (AMH)[End]
  *B603713,10 HBG 17/12/2000 Increase the picture to fix bug of **** appears in price [Begin]
  *@ ROW,125 SAY NSTYOrder   PICTURE '999999'
  @ ROW,125 SAY NSTYOrder   PICTURE '9999999'
  *B603713,10 [End]
  *B603348,1 ABD [ End ]

  ROW = ROW + 1
  
  *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
  *B603348,1     Instead of the fields. [Begin]
  *@ ROW,003 SAY lcShrt2
  *@ ROW,016 SAY nICost2 PICTURE '999999.999'
  *@ ROW,027 SAY nLan_Cost2 PICTURE '999999.999'
  *@ ROW,038 SAY nAct_Cost2 PICTURE '999999.999'
  *@ ROW,049 SAY nAct_Cost2 - nLan_Cost2 PICTURE '999999.999'
  *@ ROW,060 SAY nLan_Cost2 - nICost2 PICTURE '999999.999'
  *@ ROW,071 SAY nAct_Cost2 - nICost2 PICTURE '999999.999'
  *@ ROW,082 SAY IIF(nLan_Cost2=0,'   N/A',(nAct_Cost2 / nLan_Cost2)*100) PICTURE '999999.999'
  *@ ROW,093 SAY IIF(nICost2=0,'   N/A',(nLan_Cost2 / nICost2)*100) PICTURE '999999.999'              
  *@ ROW,104 SAY IIF(nICost2=0,'   N/A',(nAct_Cost2 / nICost2)*100) PICTURE '999999.999'              
  *@ ROW,116 SAY 'RECEIVE:'
  *@ ROW,125 SAY Receive   PICTURE '9999999'
  
  @ ROW,001 SAY lcShrt2
  @ ROW,011 SAY nICost2 PICTURE    '999999999.999'
  *B603713,10 HBG 01/22/2001 Increase the picture of cost items (AMH)[Begin]
  *@ ROW,025 SAY nLan_Cost2 PICTURE '999999.999'
  *@ ROW,036 SAY nAct_Cost2 PICTURE '999999.999'
  *@ ROW,047 SAY nAct_Cost2 - nLan_Cost2 PICTURE '999999.999'
  *@ ROW,058 SAY nLan_Cost2 - nICost2 PICTURE    '9999999999.999'
  *@ ROW,073 SAY nAct_Cost2 - nICost2 PICTURE    '9999999999.999'
  *@ ROW,088 SAY IIF(nLan_Cost2=0,' N/A',(nAct_Cost2 / nLan_Cost2)*100) PICTURE '99999.999'
  *@ ROW,098 SAY IIF(nICost2=0,' N/A',(nLan_Cost2 / nICost2)*100) PICTURE '99999.999'
  *@ ROW,108 SAY IIF(nICost2=0,' N/A',(nAct_Cost2 / nICost2)*100) PICTURE '99999.999'
  *@ ROW,118 SAY 'RECEIVE:'
  @ ROW,025 SAY nLan_Cost2 PICTURE '999999999.999'
  @ ROW,039 SAY nAct_Cost2 PICTURE '999999999.999'
  @ ROW,053 SAY nAct_Cost2 - nLan_Cost2 PICTURE '999999.999'
  @ ROW,064 SAY nLan_Cost2 - nICost2 PICTURE    '999999.999'
  @ ROW,075 SAY nAct_Cost2 - nICost2 PICTURE    '999999.999'
  @ ROW,086 SAY IIF(nLan_Cost2=0,' N/A',(nAct_Cost2 / nLan_Cost2)*100) PICTURE '99999.999'
  @ ROW,096 SAY IIF(nICost2=0,' N/A',(nLan_Cost2 / nICost2)*100) PICTURE '99999.999'
  @ ROW,106 SAY IIF(nICost2=0,' N/A',(nAct_Cost2 / nICost2)*100) PICTURE '99999.999'
  @ ROW,116 SAY 'RECEIVE:'
  *B603713,10 (AMH)[End]
  *B603713,10 HBG 17/12/2000 Increase the picture to fix bug of **** appears in price [Begin]
  *@ ROW,125 SAY Receive   PICTURE '999999'
  @ ROW,125 SAY Receive   PICTURE '9999999'
  *B603713,10 [End]
  *B603348,1 ABD [ End ]
  
  ROW = ROW + 1
  
  *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
  *B603348,1     Instead of the fields. [Begin]
  *@ ROW,003 SAY lcShrt3
  *@ ROW,016 SAY nICost3 PICTURE   '999999.999'
  *@ ROW,027 SAY nLan_Cost3 PICTURE '999999.999'
  *@ ROW,038 SAY nAct_Cost3 PICTURE '999999.999'
  *@ ROW,049 SAY nAct_Cost3 - nLan_Cost3 PICTURE '999999.999'
  *@ ROW,060 SAY nLan_Cost3 - nICost3 PICTURE    '9999999999.999'
  *@ ROW,071 SAY nAct_Cost3 - nICost3 PICTURE    '9999999999.999'
  *@ ROW,082 SAY IIF(nLan_Cost3=0,'   N/A',(nAct_Cost3 / nLan_Cost3)*100) PICTURE '99999.999'
  *@ ROW,093 SAY IIF(nICost3=0,'   N/A',(nLan_Cost3 / nICost3)*100) PICTURE '99999.999'
  *@ ROW,104 SAY IIF(nICost3=0,'   N/A',(nAct_Cost3 / nICost3)*100) PICTURE '99999.999'
  *@ ROW,116 SAY 'DAMAGED:'
  *@ ROW,125 SAY Damage   PICTURE '9999999'
  @ ROW,001 SAY lcShrt3
  @ ROW,011 SAY nICost3 PICTURE    '999999999.999'
  *B603713,10 HBG 01/22/2001 Increase the picture of cost items (AMH)[Begin]
  *@ ROW,025 SAY nLan_Cost3 PICTURE '999999.999'
  *@ ROW,036 SAY nAct_Cost3 PICTURE '999999.999'
  *@ ROW,047 SAY nAct_Cost3 - nLan_Cost3 PICTURE  '999999.999'
  *@ ROW,058 SAY nLan_Cost3 - nICost3 PICTURE     '9999999999.999'
  *@ ROW,073 SAY nAct_Cost3 - nICost3 PICTURE     '9999999999.999'
  *@ ROW,088 SAY IIF(nLan_Cost3=0,' N/A',(nAct_Cost3 / nLan_Cost3)*100) PICTURE '99999.999'
  *@ ROW,098 SAY IIF(nICost3=0,' N/A',(nLan_Cost3 / nICost3)*100) PICTURE '99999.999'
  *@ ROW,108 SAY IIF(nICost3=0,' N/A',(nAct_Cost3 / nICost3)*100) PICTURE '99999.999'
  *@ ROW,118 SAY 'DAMAGED:'
  @ ROW,025 SAY nLan_Cost3 PICTURE '999999999.999'
  @ ROW,039 SAY nAct_Cost3 PICTURE '999999999.999'
  @ ROW,053 SAY nAct_Cost3 - nLan_Cost3 PICTURE  '999999.999'
  @ ROW,064 SAY nLan_Cost3 - nICost3 PICTURE     '999999.999'
  @ ROW,075 SAY nAct_Cost3 - nICost3 PICTURE     '999999.999'
  @ ROW,086 SAY IIF(nLan_Cost3=0,' N/A',(nAct_Cost3 / nLan_Cost3)*100) PICTURE '99999.999'
  @ ROW,096 SAY IIF(nICost3=0,' N/A',(nLan_Cost3 / nICost3)*100) PICTURE '99999.999'
  @ ROW,106 SAY IIF(nICost3=0,' N/A',(nAct_Cost3 / nICost3)*100) PICTURE '99999.999'
  @ ROW,116 SAY 'DAMAGED:'
  *B603713,10 (AMH)[End]
  *B603713,10 HBG 17/12/2000 Increase the picture to fix bug of **** appears in price [Begin]
  *@ ROW,125 SAY Damage   PICTURE '999999'
  @ ROW,125 SAY Damage   PICTURE '9999999'
  *B603713,10 [End]
  *B603348,1 ABD [ End ]
 
  ROW = ROW + 1
  
  *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
  *B603348,1     Instead of the fields. [Begin]
  *@ ROW,003 SAY lcShrt4
  *@ ROW,016 SAY nICost4 PICTURE '999999.999'
  *@ ROW,027 SAY nLan_Cost4 PICTURE '999999.999'
  *@ ROW,038 SAY nAct_Cost4 PICTURE '999999.999'
  *@ ROW,049 SAY nAct_Cost4 - nLan_Cost4 PICTURE '999999.999'
  *@ ROW,060 SAY nLan_Cost4 - nICost4 PICTURE '999999.999'
  *@ ROW,071 SAY nAct_Cost4 - nICost4 PICTURE '999999.999'
  *@ ROW,082 SAY IIF(nLan_Cost4=0,'   N/A',(nAct_Cost4 / nLan_Cost4)*100) PICTURE '999999.999'
  *@ ROW,093 SAY IIF(nICost4=0,'   N/A',(nLan_Cost4 / nICost4)*100) PICTURE '999999.999'              
  *@ ROW,104 SAY IIF(nICost4=0,'   N/A',(nAct_Cost4 / nICost4)*100) PICTURE '999999.999'              
  *@ ROW,116 SAY 'CANCEL :'
  *@ ROW,125 SAY Cancel    PICTURE '9999999'
  
  @ ROW,001 SAY lcShrt4
  @ ROW,011 SAY nICost4 PICTURE    '999999999.999'
  *B603713,10 HBG 01/22/2001 Increase the picture of cost items (AMH)[Begin]
  *@ ROW,025 SAY nLan_Cost4 PICTURE '999999.999'
  *@ ROW,036 SAY nAct_Cost4 PICTURE '999999.999'
  *@ ROW,047 SAY nAct_Cost4 - nLan_Cost4 PICTURE '999999.999'
  *@ ROW,058 SAY nLan_Cost4 - nICost4 PICTURE '9999999999.999'
  *@ ROW,073 SAY nAct_Cost4 - nICost4 PICTURE '9999999999.999'
  *@ ROW,088 SAY IIF(nLan_Cost4=0,' N/A',(nAct_Cost4 / nLan_Cost4)*100) PICTURE '99999.999'
  *@ ROW,098 SAY IIF(nICost4=0,' N/A',(nLan_Cost4 / nICost4)*100) PICTURE '99999.999'
  *@ ROW,108 SAY IIF(nICost4=0,' N/A',(nAct_Cost4 / nICost4)*100) PICTURE '99999.999'
  *@ ROW,118 SAY 'CANCEL :'
  @ ROW,025 SAY nLan_Cost4 PICTURE '999999999.999'
  @ ROW,039 SAY nAct_Cost4 PICTURE '999999999.999'
  @ ROW,053 SAY nAct_Cost4 - nLan_Cost4 PICTURE '999999.999'
  @ ROW,064 SAY nLan_Cost4 - nICost4 PICTURE '999999.999'
  @ ROW,075 SAY nAct_Cost4 - nICost4 PICTURE '999999.999'
  @ ROW,086 SAY IIF(nLan_Cost4=0,' N/A',(nAct_Cost4 / nLan_Cost4)*100) PICTURE '99999.999'
  @ ROW,096 SAY IIF(nICost4=0,' N/A',(nLan_Cost4 / nICost4)*100) PICTURE '99999.999'
  @ ROW,106 SAY IIF(nICost4=0,' N/A',(nAct_Cost4 / nICost4)*100) PICTURE '99999.999'
  @ ROW,116 SAY 'CANCEL :'
  *B603713,10 (AMH)[End]
  *B603713,10 HBG 17/12/2000 Increase the picture to fix bug of **** appears in price [Begin]
  *@ ROW,125 SAY Cancel    PICTURE '999999'
  @ ROW,125 SAY Cancel    PICTURE '9999999'
  *B603713,10 [End]
  *B603348,1 ABD [ End ]

  ROW = ROW + 1
  
  *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
  *B603348,1     Instead of the fields. [Begin]
  *@ ROW,003 SAY lcShrt5
  *@ ROW,016 SAY nICost5 PICTURE '999999.999'
  *@ ROW,027 SAY nLan_Cost5 PICTURE '999999.999'
  *@ ROW,038 SAY nAct_Cost5 PICTURE '999999.999'
  *@ ROW,049 SAY nAct_Cost5 - nLan_Cost5 PICTURE '999999.999'
  *@ ROW,060 SAY nLan_Cost5 - nICost5 PICTURE '999999.999'
  *@ ROW,071 SAY nAct_Cost5 - nICost5 PICTURE '999999.999'
  *@ ROW,082 SAY IIF(nLan_Cost5=0,'   N/A',(nAct_Cost5 / nLan_Cost5)*100) PICTURE '999999.999'
  *@ ROW,093 SAY IIF(nICost5=0,'   N/A',(nLan_Cost5 / nICost5)*100) PICTURE '999999.999'              
  *@ ROW,104 SAY IIF(nICost5=0,'   N/A',(nAct_Cost5 / nICost5)*100) PICTURE '999999.999'              
  *@ ROW,116 SAY 'OPEN   :'
  *@ ROW,125 SAY Open    PICTURE '9999999'

  @ ROW,001 SAY lcShrt5
  @ ROW,011 SAY nICost5 PICTURE    '999999999.999'
  *B603713,10 HBG 01/22/2001 Increase the picture of cost items (AMH)[Begin]
  *@ ROW,025 SAY nLan_Cost5 PICTURE '999999.999'
  *@ ROW,036 SAY nAct_Cost5 PICTURE '999999.999'
  *@ ROW,047 SAY nAct_Cost5 - nLan_Cost5 PICTURE '999999.999'
  *@ ROW,058 SAY nLan_Cost5 - nICost5 PICTURE '9999999999.999'
  *@ ROW,073 SAY nAct_Cost5 - nICost5 PICTURE '9999999999.999'
  *@ ROW,088 SAY IIF(nLan_Cost5=0,' N/A',(nAct_Cost5 / nLan_Cost5)*100) PICTURE '99999.999'
  *@ ROW,098 SAY IIF(nICost5=0,' N/A',(nLan_Cost5 / nICost5)*100) PICTURE '99999.999'
  *@ ROW,108 SAY IIF(nICost5=0,' N/A',(nAct_Cost5 / nICost5)*100) PICTURE '99999.999'
  *@ ROW,118 SAY 'OPEN   :'
  @ ROW,025 SAY nLan_Cost5 PICTURE '999999999.999'
  @ ROW,039 SAY nAct_Cost5 PICTURE '999999999.999'
  @ ROW,053 SAY nAct_Cost5 - nLan_Cost5 PICTURE '999999.999'
  @ ROW,064 SAY nLan_Cost5 - nICost5 PICTURE '999999.999'
  @ ROW,075 SAY nAct_Cost5 - nICost5 PICTURE '999999.999'
  @ ROW,086 SAY IIF(nLan_Cost5=0,' N/A',(nAct_Cost5 / nLan_Cost5)*100) PICTURE '99999.999'
  @ ROW,096 SAY IIF(nICost5=0,' N/A',(nLan_Cost5 / nICost5)*100) PICTURE '99999.999'
  @ ROW,106 SAY IIF(nICost5=0,' N/A',(nAct_Cost5 / nICost5)*100) PICTURE '99999.999'
  @ ROW,116 SAY 'OPEN   :'
  *B603713,10 (AMH)[End]
  *B603713,10 HBG 17/12/2000 Increase the picture to fix bug of **** appears in price [Begin]
  *@ ROW,125 SAY Open    PICTURE '999999'
  @ ROW,125 SAY Open    PICTURE '9999999'
  *B603713,10 [End]
  *B603348,1 ABD [ End ]

  ROW = ROW + 2

  *-- Check if this po# has a bom on file.
  xPo=PO
  IF ! SEEK('I'+xPo,'CTKTBOM')
    SKIP
    LOOP
  ENDIF
  *--Print P/O bom.
  DO lpPOBom
  SELECT &PoTemp
  SKIP
  ROW=99      && PRINT ON NEXT PAGE UPON REQUEST FROM BRIDGET DONE
ENDDO
DO ENDREPORT
SET DEVICE TO SCREEN
IF USED (PoTemp)
  USE IN (PoTemp)
  ERASE (gcWorkDir+PoTemp+'.DBF')
  ERASE (gcWorkDir+PoTemp+'.CDX')
ENDIF

*!*************************************************************
*! Name      : lpPOBom
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/1998
*! Purpose   : Print Purchase orders Bill of Matirial.
*!*************************************************************
*! Called from : POPOCS.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPOBom()
*!*************************************************************
PROCEDURE lpPOBom

IF llDyelot
  @ ROW,01 SAY REPLICATE('=',122)
  ROW=ROW+1
  @ ROW,01   SAY 'ITEM                COLOR  ....DESC............ DYELOT   UNT COST    REQ. QTY   USED QTY  EST. EXTD. COST  ACT. EXTD. COST'
ELSE
  @ ROW,09 SAY REPLICATE('=',114)
  ROW=ROW+1
  @ ROW,09   SAY 'ITEM                COLOR  ....DESC............  UNT COST    REQ. QTY   USED QTY  EST. EXTD. COST  ACT. EXTD. COST'
ENDIF

ROW=ROW+1

SELECT CTKTBOM
XPRV_TYP   = SPACE(1)

*B606645,1 KHM 11/14/2002 (Begin) Initializing OldTyp with Typ and adding a new variable to hold
*B606645,1                the old value of the Typ
*OldTyp     = SPACE(1)
OldTyp     = Typ
lcMOldTyp  = SPACE(1)
*B606645,1 KHM 11/14/2002 (End)

lcOldCtg   = cCatgtyp
STORE 0 TO lnReqQty,lnUsedQty,lnEstExCst,lnActExCst
STORE 0 TO lnTotReqQty,lnTotUsedQty,lnTotEstExCst,lnTotActExCst
STORE 0 TO XGTot1,XGTot2,XGTot3,XGTot4

DO WHILE .T.
  IF ROW >= 55
    PAGENO = PAGENO + 1
    
    *B606622,1  AMH Change variable name from xtitle to lcrptitle [Start]
    *DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    DO RPT_HDR WITH XREPORT,lcRpTITLE,R_WIDTH
    *B606622,1  AMH [End]
    
    IF llDyelot
      @ 05,01   SAY 'ITEM                COLOR  ....DESC............ DYELOT   UNT COST    REQ. QTY   USED QTY  EST. EXTD. COST  ACT. EXTD. COST'
      @ 06,01 SAY REPLICATE('-',122)
    ELSE
      @ 05,09   SAY 'ITEM                COLOR  ....DESC............  UNT COST    REQ. QTY   USED QTY  EST. EXTD. COST  ACT. EXTD. COST'
      @ 06,09 SAY REPLICATE('-',114)
    ENDIF

    ROW=07
    XPRV_TYP=SPACE(1)
    
    *B606645,1 KHM 11/14/2002 (Begin) Initializing OldTyp with Typ and adding a new variable to hold
    *B606645,1                the old value of the Typ
    *OldTyp  =SPACE(1)
    OldTyp     = Typ
    lcMOldTyp  = SPACE(1)
    *B606645,1 KHM 11/14/2002 (End)

  ENDIF

  DO WHILE (!EOF()) .AND. (CUTTKT=XPO)
    IF IIF(lcOldCtg='M',OldTyp <> TYP,lcOldCtg <> cCatgTyp)
      
      @ ROW,20 SAY ALLTRIM(EVAL('lcLong'+lcNoToPrn)) + ' TOTAL ==='+REPL('=',30-LEN(ALLTRIM(EVAL('lcLong'+lcNoToPrn)))) +'>' PICTURE '@!'
      lcOldCtg   = cCatgTyp       &&TYP            && BREAK FOR SUBTOTALS
      @ ROW,67  SAY lnTotReqQty   PICTURE '9999999.999'
      @ ROW,79  SAY lnTotUsedQty  PICTURE '999999.999'
      
      *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
      *B603348,1     Instead of the field UntCost & TotCost. [Begin]
      *@ ROW,096 SAY lnTotEstExCst PICTURE '999999.999'
      @ ROW,091 SAY lnTotEstExCst PICTURE '99999999999.999'
      *B603348,1 ABD [ End ]
      
      @ ROW,113 SAY lnTotActExCst PICTURE '999999.999'
      XGTot1 = XGTot1 + lnTotReqQty
      XGTot2 = XGTot2 + lnTotUsedQty
      XGTot3 = XGTot3 + lnTotEstExCst
      XGTot4 = XGTot4 + lnTotActExCst
      STORE 0 TO lnTotReqQty,lnTotUsedQty,lnTotEstExCst,lnTotActExCst
      ROW=ROW+1
    ENDIF

    *B606645,1 KHM 11/14/2002 (Begin) Changing the condition in case of lcOldCtg='M'.
    *IF IIF(lcOldCtg='M',OldTyp <> TYP,XPRV_TYP <> cCatgTyp)
    IF IIF(lcOldCtg='M',lcMOldTyp <> TYP,XPRV_TYP <> cCatgTyp)
    *B606645,1 KHM 11/14/2002 (End)
    
      lcHdrToSay = '< ' +ALLTRIM(EVAL('lcLong'+Typ))+' >'
      IF llDyelot
        @ ROW,01 SAY PADC(lcHdrToSay,122,'-')
      ELSE
        @ ROW,09 SAY PADC(lcHdrToSay,114,'-')
      ENDIF
      
      ROW=ROW+1
      XPRV_TYP  = cCatgTyp         &&TYP
      lcOldCtg  = cCatgTyp
      lcNoToPrn = TYP
      OldTyp    = TYP
    ENDIF

    IF cCatGTyp = 'M'
      *B606645,1 KHM 11/14/2002 (Begin) Commented out as the MFG code is included in the Desc.
      *@ ROW,33 SAY MFGCODE
      *B606645,1 KHM 11/14/2002 (End)
      
      *B603348,1 ABD Remark The next Line & Print only 15 Character From The Desc. [ Begin ]
      *@ ROW,36 SAY DESC    PICTURE '@!' 

      *B606645,1 KHM 11/14/2002 (Begin) Changing the col to be 33 instead of 36.
      *@ ROW,36 SAY SUBSTR(DESC,1,15) PICTURE '@!'       
      @ ROW,33 SAY SUBSTR(DESC,1,15) PICTURE '@!'       
      *B606645,1 KHM 11/14/2002 (End)
      
      *B603348,1 ABD [ End ]
    ELSE
      IF llDyelot
        @ ROW,01 SAY ITEM
        @ ROW,21 SAY ICLR
        *B603348,1 ABD Remark The next Line & Print only 15 Character From The Desc.  & 
        *B603348,1     Move Dyelot To the Left. [ Begin ]
        *@ ROW,28 SAY DESC    PICTURE '@!' 
        *@ ROW,49 SAY DYELOT
        @ ROW,28 SAY SUBSTR(DESC,1,15) PICTURE '@!' 
        @ ROW,44 SAY DYELOT
        *B603348,1 ABD [ End ]
      ELSE
        @ ROW,09 SAY ITEM
        @ ROW,29 SAY ICLR
        *B603348,1 ABD Remark The next Line & Print only 15 Character From The Desc. [ Begin ]
        *@ ROW,36 SAY DESC    PICTURE '@!' 
        @ ROW,36 SAY SUBSTR(DESC,1,15) PICTURE '@!'       
        *B603348,1 ABD [ End ]

      ENDIF
    ENDIF
    *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture to '999999.999'
    *B603348,1     Instead of the field UntCost . [Begin]
    *@ ROW,58 SAY UNTCOST PICTURE '999.999'
    *B603713,10 HBG 17/12/2000 Fix bug of **** appears in unit cost [Begin]
    *@ ROW,55 SAY UNTCOST PICTURE  '9999999.999'    
    @ ROW,52 SAY UNTCOST PICTURE  '99999999999.999'    
    *B603713,10 [End]
    *B603348,1 ABD [ End ] 
  
  
    = lfCalculate()
    @ ROW,67  SAY lnReqQty   PICTURE '9999999.999'
    @ ROW,79  SAY lnUsedQty  PICTURE '999999.999'

    *B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
    *B603348,1     to '99999999999.999' Instead of the fields [Begin]
    *@ ROW,096 SAY lnEstExCst PICTURE '999999.999'    
    @ ROW,091 SAY lnEstExCst PICTURE '99999999999.999'
    *B603348,1 ABD [ End ]

    @ ROW,113 SAY lnActExCst PICTURE '999999.999'
    
    ROW=ROW+1

    SKIP
    IF ROW >= 55
      EXIT
    ENDIF
  ENDDO

  IF EOF() .OR. CTKTBOM->CUTTKT <> XPO
    EXIT
  ENDIF
ENDDO

*--SUBTOTAL FOR LAST TYPE.
@ ROW,20 SAY ALLTRIM(EVAL('lcLong'+lcNoToPrn)) + ' TOTAL ==='+REPL('=',30-LEN(ALLTRIM(EVAL('lcLong'+lcNoToPrn)))) +'>'  PICTURE '@!'
@ ROW,67  SAY lnTotReqQty   PICTURE '9999999.999'
@ ROW,79  SAY lnTotUsedQty  PICTURE '999999.999'
*B603713,10 HBG 17/12/2000 Fix bug of **** appears [Begin]
*@ ROW,096 SAY lnTotEstExCst PICTURE '999999.999'
@ ROW,091 SAY lnTotEstExCst PICTURE '99999999999.999'
*B603713,10 [End]
@ ROW,113 SAY lnTotActExCst PICTURE '999999.999'
XGTot1 = XGTot1 + lnTotReqQty
XGTot2 = XGTot2 + lnTotUsedQty
XGTot3 = XGTot3 + lnTotEstExCst
XGTot4 = XGTot4 + lnTotActExCst
ROW=ROW+1

*B604465,1  MHM 09/05/2001 [Start]
*IF llDyelot
*  @ 06,01 SAY REPLICATE('-',122)
*ELSE
*  @ 06,09 SAY REPLICATE('-',114)
*ENDIF

IF llDyelot
  @ ROW,01 SAY REPLICATE('-',122)
ELSE
  @ ROW,09 SAY REPLICATE('-',114)
ENDIF
*B604465,1  MHM 09/05/2001 [End]

ROW=ROW+1
*-- GRAND TOTAL FOR THIS PO.
@ ROW,44 SAY 'TOTAL COSTS  ===>'
@ ROW,67  SAY XGTot1 PICTURE '9999999.999'
@ ROW,79  SAY XGTot2 PICTURE '999999.999'

*B603348,1 ABD Remark The next Line & Move Them To The Left & Change The Picture 
*B603348,1     to '99999999999.999' Instead of the fields [Begin]
*@ ROW,096 SAY XGTot3 PICTURE '999999.999'
@ ROW,091 SAY XGTot3 PICTURE '99999999999.999'
*B603348,1 ABD [ End ]

@ ROW,113 SAY XGTot4 PICTURE '999999.999'

ROW=ROW+1
RETURN

*!*************************************************************
*! Name      : lfCalculate
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/1998
*! Purpose   : Calculate REQ.QTY,USEDQTY,EST.EXTD.COST,ACT.EXTD.COST fields.
*!*************************************************************
*! Called from : POPOCS.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCalculate()
*!*************************************************************
FUNCTION lfCalculate

lcCPo = Cuttkt
STORE 0 TO lnReqQty,lnUsedQty,lnEstExCst,lnActExCst
SELECT BOMLINE
*Used index : CIMTYP+CTYPE+CTKTNO+LINENO+CBOMTYP+STYLE+SCLR+IIF(!(cCatGTyp$'MDP'),ITEM,MFGCODE)+ICLR+STR(RECNO(),7)
SEEK 'I'+'1'+lcCPo
lcForKey=IIF(EMPTY(CTKTBOM.Item),CTKTBOM.MfgCode,CTKTBOM.Item+CTKTBOM.IClr)

SUM (ItemQty),(ItemQty*UnitCost) REST ;
         WHILE CIMTYP+CTYPE+CTKTNO='I'+'1'+lcCPo ;
         FOR  IIF(!(cCatGTyp$'MDP'),ITEM,MFGCODE)+ICLR = lcForKey .AND. cBomTyp=CTKTBOM.Typ ;
         TO   lnReqQty,lnEstExCst

* Display estimated purchase price in base currency

*B606012,1 AMH Fix bug of print extd cost with different currencies [Start]
*lnEstExCst = IIF(CTKTBOM.Typ='1' .AND. SEEK(XPO,'POSHDR'),;
                 lnEstExCst/POSHDR.RATE,lnEstExCst)
STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
IF SEEK('P'+XPO,'POSHDR')
  lcPExSign = gfGetExSin(@lcPUntSin,POSHDR.cPriceCur)
  lcDExSign = gfGetExSin(@lcDUntSin,POSHDR.cDutyCur)
  DO CASE
    CASE CTKTBOM.cCatgTyp='P'
      lnEstExCst = lnEstExCst &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit
    CASE !INLIST(CTKTBOM.cCatgTyp,'S','F','T')
      lnEstExCst = lnEstExCst &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit
  ENDCASE
ENDIF
*B606012,1 AMH [End]

SELECT BOMCOST
*B802043,1 AMM Adjust because the index has changed.
*Used index :CTKTNO+IIF(EMPTY(ITEM),"Y","N")+IIF(CCOSTTYPE$"MDP",MFGCODE,ITEM)+ICLR+STR(RECNO(),7)
*IF EMPTY(CTKTBOM.Item)
  *lcKey    = lcCPo+"Y"+CTKTBOM.MfgCode
  *lcWhlKey = 'cTktNo+"Y"+MfgCode'
*ELSE
  *lcKey    = lcCPo+"N"+CTKTBOM.Item+CtktBom.IClr
  *lcWhlKey = 'cTktNo+"N"+Item+IClr'
*ENDIF
*SEEK lcKey
*SUM nTotQty,nTotCst REST WHILE &lcWhlKey = lcKey ;
                    FOR cIMtyp='I' .AND. cBomType=CTKTBOM.Typ ;
                    TO lnUsedQty,lnActExCst 

*New Used Inex CBOMTYPE+CIMTYP+CTKTNO+ITEM+ICLR+MFGCODE+CWARECODE+CDYELOT+CRSESSION+CISESSION                                          
=SEEK(CTKTBOM.Typ+'I'+lcCPo+CTKTBOM.Item+CtktBom.IClr+CTKTBOM.MfgCode)
SUM nTotQty,nTotCst REST WHILE cBomType+cIMtyp+cTktNo+Item+IClr+MfgCode = ;
                    CTKTBOM.Typ+'I'+lcCPo+CTKTBOM.Item+CtktBom.IClr+CTKTBOM.MfgCode ;
                    TO lnUsedQty,lnActExCst 

*B802043,1 AMM end

*-Accoumulate for subtotals.
lnTotReqQty   = lnTotReqQty   + lnReqQty
lnTotUsedQty  = lnTotUsedQty  + lnUsedQty
lnTotEstExCst = lnTotEstExCst + lnEstExCst
lnTotActExCst = lnTotActExCst + lnActExCst

SELECT CTKTBOM
RETURN


*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/98
*! Purpose   : To set relation on or off when running the in range function 
*!             of selecting PO in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*!*************************************************************
*! Name      : lfvVend
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/1998
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : POPOCS.PRG
*!*************************************************************
*! Calls       : gfApVnBrow().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend

PRIVATE lcVar, lcObj
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/1998
*! Purpose   : To get the old value of the field
*!*************************************************************
*! Called from : POPOCS.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))

