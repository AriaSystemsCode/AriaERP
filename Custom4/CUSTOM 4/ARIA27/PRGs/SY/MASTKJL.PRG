*:***************************************************************************
*: Program file  : MASTKJL.PRG
*: Program desc. : MATERIALS Stock Adjustment Journal Report
*: For Report    : (@x,y SAY)
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (MA)
*: Developer     : IHB
*: DATE          : 12/29/1998 
*:***************************************************************************
*: Calls : 
*:    Procedures : None
*:    Functions  : lfvFabric,lfWOldVal,lfvWareCod,
*:               : lfvInvTran,lfInvExpr,lfwRunGrid
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MASTKJL
*:***************************************************************************
*: This Report Program is due to E301071
*:***************************************************************************
*-- Report Layout : MATERIAL INVENTORY ADJUSTMENT JOURNAL
*-- Transaction Type  (IIF(MultiWH,[A]djustments [P]hysical [T]ransfer,
*--                        [A]djustments [P]hysical))  
*-- Adjustments date range
*-- Item in the range
*-- Warehouse  (IIF(MultiWH,Warehouse Range,Only One Warehouse))  
*:***************************************************************************
*: Modifications :
*B602629,1 IHB  08/03/1999 change field Finvtadj.TotAdj to be Finvtadj.nmTotAdj
*               and modify the printing picture to include decimales           
*B802013,1 IHB  08/03/1999 variable not found 'TotAdj' is included
*               in B602629,1
*B802245,1 HDM 05/22/1999 Increase picture for old. adj. total fields
*:B603008,1 MAN 06/22/99 Fixed the location filter to work for source, and 
*:B603008,1              target locations.
*:B:802465,1 SSH 01/08/99 Alignment Dylot
*:***************************************************************************

R_TITLE = 'MATERIAL INVENTORY ADJUSTMENT JOURNAL'   && report title

*-- if multi locations.
IF llMultWare
  lnStrtCol = 7           &&Starting Column

ELSE  && else single location.

  lnStrtCol = 0    

ENDIF  && end if multi locations.

*-- If company is not multilocations display only one location,
*-- and don't display transaction type transfer.
*-- A relation between workfile and fabric to complete information .
SELECT FInvtAdj
GOTO TOP

*-- lcRpInvExp : Stores value of transaction type.
*-- lcRpexp : passed filter expression from option grid.

*-- if user choose any transaction type(s) ,Modifying filter expression
IF !EMPTY(lcRpInvExp)
  lcRpExp = lcRpExp + ' AND (FInvtAdj.Type $ lcRpInvExp)'   && because lcRpInvExp is a variable.

ENDIF  && end if user choose any transaction type(s) 
*B603008,1 Add the Location to the filter exprission Start
IF !EMPTY(lcRpLoc)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + " .AND."
  ENDIF 
  lcRpExp = lcRpExp + " (FInvtAdj.cFromWare==lcRpLoc OR FInvtAdj.cToWare==lcRpLoc)"
ENDIF  
*B603008,1 End
      
SELECT FINVTADJ                && the file in which data is collected from.  
LOCATE ALL FOR &lcRpExp        && locates the 1st rec. in scope.
*-- if no records matches the selection criteria.
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN

ENDIF  && end if no records matches the selection criteria.

lcTempFile = gfTempName()    &&global function gives temporarily name for the workfile.
COPY REST TO (gcWorkDir+lcTempFile) FOR &lcRpExp    && appends all needed records.
=gfOpenFile(gcWorkDir+lcTempFile,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
INDEX ON Fabric+Color+cFromWare TAG (lcTempFile)
SET RELATION TO Fabric+Color INTO Fabric    && to bring rest of fabric data from fabric file

XTOTAL = 0.00     && subtotal
GTOTAL = 0.00     && grand total
PAGENO = 0        && page number
ROW    = 99       && row is used for formulating the different lines  
XTIME  = TIME()   && time 
BREAK  = 'FABRIC' && data grouping by fabric

GOTO TOP    && in workfile
HBREAK = SPACE(1)

*-- if there's no breake
IF !EMPTY(BREAK)
  HBREAK = &BREAK

ENDIF  && end if there's no breake

*-- print/preview  code
*-- loop workfile to print data information.
SET DEVICE TO PRINT
DO WHILE .T.
  *-- if new page (row is initially 99 and for new page also) 
  IF ROW >=53
    PAGENO  = PAGENO + 1
    XREPORT = 'MASTKJL'
    XTITLE  = ' '
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH     && report header
    *-- if multi locations.
    IF llMultWare
      *-- HDM B802245,1 [Start]
      *@ 06,00 SAY 'WAREHS FABRIC               COLOR        REASON...................  DATE      TYP    OLD. QTY ADJUSTMENT   NEW QTY.'
      *           '----5----10---15---20---25---30---35---40---45---50---55---60---65---70---75---80---85---90---95---100--105--110--115--120--125--130'  
     @ 06,00 SAY 'WAREHS FABRIC               COLOR        REASON...................  DATE      TYP     OLD. QTY    ADJUSTMENT       NEW QTY.'
     *--HDM B802245,1 [End]
    ELSE  && else single location.
      @ 06,00 SAY 'FABRIC               COLOR        REASON...................  DATE      TYP     OLD. QTY    ADJUSTMENT       NEW QTY.'
    
    ENDIF  && end if multi locations.
    *:B:802465,1 SSH 01/08/99 Alignment Dylot
    *@ 07,00+lnStrtCol SAY 'DESCRIPTION......... DESCRIPTION.' + IIF(llUseDyes,' DYELOT','')
    @ 07,00+lnStrtCol SAY 'DESCRIPTION......... DESCRIPTION.                  ' + IIF(llUseDyes,' DYELOT','')
    *:B:802465,1 SSH (END)
    ROW = 08
  ENDIF
  
  *-- if there is a breake (data grouping) ,print subtotal 
  IF !EMPTY(BREAK) AND &BREAK <> HBREAK
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',132)
    ROW = ROW + 1
    @ ROW,000 SAY '* SUB TOTAL * '
    @ ROW,020 SAY HBREAK
    @ ROW,089+lnStrtCol SAY XTOTAL PICTURE '99999999.999'
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',132)
    ROW = ROW + 2
    GTOTAL = GTOTAL + XTOTAL
    XTOTAL = 0
    HBREAK = &BREAK
  
  ENDIF  && end if there is a breake (data grouping) ,print subtotal 
  
  *-- if eof() exits this loop  
  IF EOF()
    EXIT
  
  ENDIF  && end if eof()

  *-- if end of page , loop for a new page  
  IF ROW >= 53
    ROW = 99
    LOOP
  
  ENDIF  && end if end of page

  SELECT (lcTempFile)
  XCLR = COLOR
  ClrDesc =gfCodDes(XCLR,'COLOR')
  
  SELECT (lcTempFile)
  @ ROW,000 SAY REPLICATE('-',132)
  ROW = ROW + 1
  xNewQty = 0

  *-- if multilocatons and transaction type is not transfer 
  IF llMultWare .AND. Type <> 'T'
    @ ROW,00 SAY cFromWare
  
  ENDIF  && end if multilocatons and transaction type is not transfer 
  
  @ ROW,000+lnStrtCol SAY Fabric
  @ ROW,021+lnStrtCol SAY Color
  @ ROW,034+lnStrtCol SAY cReason
  @ ROW,061+lnStrtCol SAY Date
  
  *-- print transaction type and some other columns according to 
  *-- type column value in workfile.
  DO CASE
    CASE Type = 'A'
      @ ROW,071+lnStrtCol SAY 'ADJ'
      
      *B602629,1 IHB [start]
      *@ ROW,078+lnStrtCol SAY OldQty PICTURE '99999999'
      *@ ROW,089+lnStrtCol SAY TotAdj PICTURE '99999999'
      *xNewQty = OldQty + TotAdj
      *xTotal  = xTotal + TotAdj
      *@ ROW,100+lnStrtCol SAY xNewQty PICTURE '99999999'
      *--B802245,1 HDM 05/22/1999 [Start]
      *@ ROW,078+lnStrtCol SAY OldQty PICTURE '99999.99'
      @ ROW,075+lnStrtCol SAY OldQty PICTURE '99999999.999'
      *--B802245,1 HDM 05/22/1999[End]
      *--B802245,1 HDM 05/22/1999[Start]
      *@ ROW,089+lnStrtCol SAY nmTotAdj PICTURE '99999.99'
      @ ROW,089+lnStrtCol SAY nmTotAdj PICTURE '99999999.999'
      *--B802245,1 HDM 05/22/1999[End]
      
      xNewQty = OldQty + nmTotAdj
      xTotal  = xTotal + nmTotAdj
      *--B802245,1 HDM 05/22/1999 [Start]
      *@ ROW,100+lnStrtCol SAY xNewQty PICTURE '99999.99'
      @ ROW,104+lnStrtCol SAY xNewQty PICTURE '99999999.999'
      *--B802245,1 HDM 05/22/1999[End]
      *B602629,1 IHB [end]
      
    CASE Type = 'P'
      @ ROW,071+lnStrtCol SAY 'PHY'

      *B602629,1 IHB [start]
      *@ ROW,078+lnStrtCol SAY OldQty PICTURE '99999999'
      *xNewQty = TotAdj
      *@ ROW,100+lnStrtCol SAY xNewQty PICTURE '99999999'
      @ ROW,078+lnStrtCol SAY OldQty PICTURE '99999.99'
      xNewQty = nmTotAdj
      @ ROW,100+lnStrtCol SAY xNewQty PICTURE '99999.99'
      *B602629,1 IHB [end]
      
    CASE Type = 'T'
      @ ROW,071+lnStrtCol SAY 'TRN'
      @ Row,082 SAY 'FROM:'
      @ Row,087 SAY cFromWare
      @ Row,096 SAY 'TRANSFER'
      @ Row,106 SAY 'TO:'
      @ Row,109 SAY cToWare
  
  ENDCASE  &&end of selecting columns from workfile according to type

  ROW = ROW + 1     
  @ ROW,000+lnStrtCol SAY Fabric.Desc
  @ ROW,021+lnStrtCol SAY ClrDesc

  *-- if company uses dyelots

  IF llUseDyes
    *:B:802465,1 SSH 01/08/99 Alignment Dylot
    *@ ROW,034+lnStrtCol SAY Dyelot
    @ ROW,052+lnStrtCol SAY Dyelot
    *:B:802465,1 SSH (END)
  ENDIF  && end if company uses dyelots
  
  *-- if transaction type is transfer , print further detail
  IF Type = 'T'

     *B602629,1 IHB [start]
    *@ ROW,078+lnStrtCol SAY OldQty             PICTURE '99999999'
    *@ ROW,089+lnStrtCol SAY TotAdj             PICTURE '99999999'
    *@ ROW,100+lnStrtCol SAY nOldToQty + TotAdj PICTURE '99999999'
     @ ROW,078+lnStrtCol SAY OldQty             PICTURE '99999.99'
     @ ROW,089+lnStrtCol SAY nmTotAdj           PICTURE '99999.99'
     @ ROW,100+lnStrtCol SAY nOldToQty+nmTotAdj PICTURE '99999.99'
     *B602629,1 IHB [end]

  ENDIF  && end if transaction type is transfer 
  ROW = ROW + 1

  SELECT &lcTempFile
  SKIP
ENDDO   && end loop workfile to print data information.

ROW = ROW + 2
@ ROW,00 SAY REPLICATE('*',132)
ROW = ROW + 1
@ ROW,000 SAY '*GRAND TOTAL*'

*B602629,1 IHB [start]
*@ ROW,089+lnStrtCol SAY GTOTAL PICTURE '99999999'

*--B802245,1 HDM 05/22/1999 [Start]
*@ ROW,089+lnStrtCol SAY GTOTAL PICTURE '99999.99'
@ ROW,088+lnStrtCol SAY GTOTAL PICTURE '999999999.999'
*--B802245,1 HDM 05/22/1999 [End]
*B602629,1 IHB [end]

ROW = ROW + 1
@ ROW,00 SAY REPLICATE('*',132)
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN      && return to the caller which is gfopgrid (option grid program)
*-- end of report code...
*--------------------- Functions' Section ---------------------
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfvFabric
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : Validation function for the Fabric field
*!*************************************************************
*! Called from : Fabric field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvFabric
lcObjName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
*-- if The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal

ENDIF  && end of if   
*-- end of lfvFabric function

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : Old Value for fbric/warehouse (in OG) 
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfWOldVal
*!*************************************************************
*-- SYS(18) Returns the name in upper-case of the memory variable, 
*-- array element or field used to create the current @ ... GET control.
FUNCTION lfWOldVal
lcOldVal = EVALUATE(SYS(18))
*-- end of lfWOldVal function .

*!*************************************************************
*! Name      : lfvWareCod
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvWareCod
PRIVATE lcObjName , lcObjVal
lcObjName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Warehouse he entered is not in the
*--file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcOldVal , lcObjVal)
  &lcObjName = lcObjVal

ENDIF && end of if    
*-- end of lfvWareCod function .

*!*************************************************************
*! Name      : lfvInvTran
*! Developer : IHB
*! Date      : 10/19/98
*! Purpose   : If user Press Inventory transaction button.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvInvTran()
*!*************************************************************
FUNCTION lfvInvTran
= gfMover(@laRpSorInv,@laRpTarInv,'Select Inventory Transaction type',.T.,'')  && call mover function.
= lfInvExpr()
*-- end of lfvInvTran function. 

*!*************************************************************
*! Name      : lfInvExpr
*! Developer : IHB
*! Date      : 11/21/1998
*! Purpose   : Evaluate Company expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lfvInvTran
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfInvExpr()
*!*************************************************************
FUNCTION lfInvExpr
PRIVATE laTarget

*-- Copy Used array.
*-- if target array has no items 
IF EMPTY(laRpTarInv)
  = ACOPY(laRpSorInv,laTarget)

ELSE  && there're items
  = ACOPY(laRpTarInv,laTarget)

ENDIF  && end if target array has no items 
  
= ASORT(laTarget)  && Sort array.
lcRpInvExp = ''

*-- loop to get inventory transaction type expression.
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpInvExp = IIF(EMPTY(lcRpInvExp),LEFT(laTarget[lnI],1),;
                    lcRpInvExp + ','+LEFT(laTarget[lnI],1))

ENDFOR   && end loop to get inventory transaction type expression.
*-- end of lfInvExpr function .

*!*************************************************************
*! Name      : lfwRunGrid
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : Initialzes source and target arrays for trans.type.
*!             Depend on company setting whether multiwarehouse
*!             or not, we'll have:(Adjustment/Physical/Transfer)
*!             or (Adjustment/Physical) respectively .      
*!             Note that this fn. is called from the report gen.
*!             (OG. when fn.) 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid

R_WIDTH = 'W'          && Width of report either N',W or XW

*-- Fill date field in OG. [begin]
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'FINVTADJ.DATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate)+'|'+DTOC(gdSysDate)

ENDIF
*-- Fill date field in OG. [end  ]

*-- Define Mover for trans. type arrays.  [begin]
lnArrayLen = IIF(llMultWare,3,2)
DECLARE laRpSorInv[lnArrayLen,1],laRpTarInv[lnArrayLen,1]
STORE 'Adjustments' TO laRpSorInv[1],laRpTarInv[1]
STORE 'Physical'    TO laRpSorInv[2],laRpTarInv[2]
IF lnArrayLen = 3
  STORE 'Transfer' TO laRpSorInv[3],laRpTarInv[3]

ENDIF  
*-- Define Mover for trans. type arrays.  [end  ]
*-- end of lfwRunGrid function .
