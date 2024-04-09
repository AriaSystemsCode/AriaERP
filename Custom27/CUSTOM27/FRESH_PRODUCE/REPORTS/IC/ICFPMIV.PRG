****************************************************************************
*: Program file      : ICFPMIV.PRG
*: Program desc.     : Monthly Inventory Valuation                  
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : TAK
*: Date              : 10/23/2000
*:**************************************************************************

*--ckeck on warehouse.
lcWareCode = ""
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'WAREHOUS.CWARECODE'),1)
IF lnPosition > 0
  IF EMPTY(laOGFxFlt[lnPosition,6])
    =gfModalGen('TRM34020B34000','DIALOG','Warehouse'+'|'+' ')      
    SET DEVI TO SCREEN
    RETURN
  ENDIF
  lcWareCode = laOGFxFlt[lnPosition,6]
ELSE
  RETURN
ENDIF

lnAdoCost = IIF(lcWareCode = 'FP-DSG' AND lcTran='S',lnAdoCost,0)

lcCurDate = lfGetMon(month(gdSysDate))+' '+ALLT(STR(day(gdSysDate)))+', '+ALLT(STR(year(gdSysDate)))

lcWareName=IIF(SEEK(lcWareCode,'WAREHOUS'),ALLT(WAREHOUS.CDesc),"")

lcRepStrg = lcWareName+' '+IIF(lcTran='W','Work In Progress','Stock')+' as of '+lcCurDate

llPSub = (lcStySub='Y')


*-- To get the selected seasons
lcSeasons = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.SEASON'),1)
IF lnPosition > 0
  lcSeasons = laOGFxFlt[lnPosition,6] 
ENDIF


lcTmpStyle = gfTempName()

WAIT WINDOW 'Prepering styles...' NOWAIT
SELECT STYLE
COPY STRU TO (gcWorkDir+lcTmpStyle)
=gfOpenFile(gcWorkDir+lcTmpStyle,'','EX')
INDEX ON Style TAG (lcTmpStyle)
SET RELATION TO Style INTO STYLE

SELECT STYDYE
SET RELATION TO Style INTO STYLE
=SEEK(lcWareCode)
SCAN REST WHILE cWareCode = lcWareCode ;
     FOR IIF(lcTran='W',TotWip <> 0,TotStk <> 0) AND IIF(!EMPTY(lcSeasons), STYLE.Season $ lcSeasons , .T. )
  WAIT WINDOW Style NOWAIT
  
  SELECT (lcTmpStyle)
  APPEND BLANK
  REPLACE Style   WITH STYDYE.Style,;
          Season  WITH STYLE.Season,;
          Stk1    WITH IIF(lcTran='W',STYDYE.wip1,STYDYE.stk1),;
          Stk2    WITH IIF(lcTran='W',STYDYE.wip2,STYDYE.stk2),;
          Stk3    WITH IIF(lcTran='W',STYDYE.wip3,STYDYE.stk3),;
          Stk4    WITH IIF(lcTran='W',STYDYE.wip4,STYDYE.stk4),;
          Stk5    WITH IIF(lcTran='W',STYDYE.wip5,STYDYE.stk5),;
          Stk6    WITH IIF(lcTran='W',STYDYE.wip6,STYDYE.stk6),;
          Stk7    WITH IIF(lcTran='W',STYDYE.wip7,STYDYE.stk7),;
          Stk8    WITH IIF(lcTran='W',STYDYE.wip8,STYDYE.stk8),;
          TotStk  WITH IIF(lcTran='W',STYDYE.Totwip,STYDYE.TotStk)

   lnCost = 0
   DO CASE
     CASE lcTran='W' AND lcWarecode='FP-BLD' AND STYLE.Make
       lnCost = STYLE.nmCost4
     CASE lcTran='W' and lcWarecode='FP-GAR' AND STYLE.Make
       lnCost = STYLE.nmCost1+STYLE.nmCost2
     OTHE
       lnCost = STYLE.TotCost+lnAdoCost
   ENDCASE

   REPLACE TotCost WITH lnCost
   
ENDSCAN
WAIT CLEAR
SET RELATION TO

SELECT (lcTmpStyle)
GO BOTT
lcLastSty = Style
GO TOP

= gfDispRe(lcRpForm)

IF USED(lcTmpStyle)
  SELECT (lcTmpStyle)
  USE
  ERASE(gcWorkDir+lcTmpStyle+'.DBF')
  ERASE(gcWorkDir+lcTmpStyle+'.CDX')
ENDIF
RETURN




********************************************************
FUNCTION lfvWare
PRIVATE lcWarFld,lcWarhous,lnSelFile,lcWareTag

lcWarFld   = VARREAD()
lcWareCode = EVAL(lcWarFld)

lnSelFile = SELECT(0)
SELECT WAREHOUS
IF !EMPTY(lcWareCode) .AND. ('?' $ lcWareCode .OR. !SEEK(lcWareCode , 'WAREHOUS'))
  lcWareCode =gfBrowWare(.T.)  
ENDIF
&lcWarFld = lcWareCode
IF lcWareCode = 'FP-DSG' AND lcTran='S'
  lnAdoCost = IIF(lnAdoCost=0,1.10,lnAdoCost)
  SHOW GET lnAdoCost ENABLE
ELSE
  lnAdoCost = 0
  SHOW GET lnAdoCost DISABLE
ENDIF
SELECT (lnSelFile)
RETURN


********************************************
FUNCTION lfGetMon
PARA lnMonth

lcMonth = ""
DO CASE
  CASE lnMonth=1
    lcMonth = "January"
  CASE lnMonth=2
    lcMonth = "February"
  CASE lnMonth=3
    lcMonth = "March"
  CASE lnMonth=4
    lcMonth = "April"
  CASE lnMonth=5
    lcMonth = "May"
  CASE lnMonth=6
    lcMonth = "June"
  CASE lnMonth=7
    lcMonth = "July"
  CASE lnMonth=8
    lcMonth = "August"
  CASE lnMonth=9
    lcMonth = "September"
  CASE lnMonth=10
    lcMonth = "October"
  CASE lnMonth=11
    lcMonth = "November"
  CASE lnMonth=12                    
    lcMonth = "December"
ENDCASE
RETURN lcMonth

**********************************
FUNCTION lfwRepWhen
lnAdoCost = 0
SHOW GET lnAdoCost DISABLE


********************************************************
FUNCTION lfvTrans
PRIVATE lcWarhous

lcWarhous = laOGFxFlt[ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'WAREHOUS.CWARECODE'),1),6]
IF !EMPTY(lcWarhous) AND lcWarhous = 'FP-DSG' AND lcTran='S'
  lnAdoCost = IIF(lnAdoCost=0,1.10,lnAdoCost)
  SHOW GET lnAdoCost ENABLE
ELSE
  lnAdoCost = 0
  SHOW GET lnAdoCost DISABLE
ENDIF
RETURN
