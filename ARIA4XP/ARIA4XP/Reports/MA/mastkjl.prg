*:***************************************************************************
*: Program file  : MASTKJL.PRG
*: Program desc. : MATERIALS Stock Adjustment Journal Report
*: System        : Aria4XP
*: Module        : MATERIALS (MA)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: DATE          : 6/13/2006 
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
*: This Report Program is due to N037709
*:***************************************************************************
*-- Report Layout : MATERIAL INVENTORY ADJUSTMENT JOURNAL
*-- Transaction Type  (IIF(MultiWH,[A]djustments [P]hysical [T]ransfer,
*--                        [A]djustments [P]hysical))  
*-- Adjustments date range
*-- Item in the range
*-- Warehouse  (IIF(MultiWH,Warehouse Range,Only One Warehouse))  
*:***************************************************************************
*: Modifications:
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004]
*:***************************************************************************

IF llOgFltCh
  llDonprnt=.F.
lcTempFile = LOOGSCROLL.gfTempName()    &&global function gives temporarily name for the workfile.
lcWorkfile = LOOGSCROLL.gfTempName() 
lnMajor  = LEN(gfItemMask('PM','','0002'))
lnNoMajor = LEN(gfItemMask('PN','','0002'))

STORE ' .T. ' TO lcItmExp

*ITEM Filter
lcITEMFile = ''
lcITEMFile = lfCheckFilter(3, 'ITEM.CSTYMAJOR')
llITEMFltr   = !EMPTY(lcITEMFile ) AND USED(lcITEMFile ) AND RECCOUNT(lcITEMFile ) > 0
IF llITEMFltr   
  SELECT (lcITEMFile )
  INDEX ON CSTYMAJOR TAG (lcITEMFile )
  lcItmExp=lcItmExp+" AND SEEK(SUBSTR(STYLE,1,lnMajor),'"+lcITEMFile +"')"
ELSE
  IF TYPE("lcITEMFile ") = "C" AND USED(lcITEMFile )
    USE IN (lcITEMFile )
  ENDIF
  lcITEMFile = ''
ENDIF

*WAREhOUSE Filter
IF llMultWare
	lcWARFile = ''
	lcWARFile = lfCheckFilter(1, 'ITEMADJ.CFROMWARE')
	llWARFltr   = !EMPTY(lcWARFile ) AND USED(lcWARFile ) AND RECCOUNT(lcWARFile ) > 0
	IF llWARFltr   
	  SELECT (lcWARFile )
	  INDEX ON cWareCode  TAG (lcWARFile )
	  lcItmExp=lcItmExp+" AND (SEEK(CFROMWARE,'"+lcWARFile+"') OR SEEK(CTOWARE,'"+lcWARFile+"')) "
	ELSE
	  IF TYPE("lcWARFile ") = "C" AND USED(lcWARFile )
	    USE IN (lcWARFile )
	  ENDIF
	  lcWARFile = ''
	ENDIF
ENDIF

lnInvPos = lfItmPos('ITEMADJ.DATE')
IF !EMPTY(laOGFxFlt[lnInvPos ,6])
 ldStrtDPst = CTOD(SUBSTR(laOGFxFlt[lnInvPos ,6],1, ATC('|',laOGFxFlt[lnInvPos ,6])-1))
 ldEndDPst  = CTOD(SUBSTR(laOGFxFlt[lnInvPos ,6],   ATC('|',laOGFxFlt[lnInvPos ,6])+1))
 lcItmExp=lcItmExp+" AND BETWEEN(DATE,ldStrtDPst,ldEndDPst) "
ENDIF 

*-- if user choose any transaction type(s) ,Modifying filter expression
IF !EMPTY(lcRpInvExp)
  lcItmExp= lcItmExp+ ' AND (ITEMADJ.Type $ lcRpInvExp)'   
ENDIF  



gfOpenTABLE(oAriaApplication.DataDir + "ITEMADJ" , "INVTADJ",'SH', , .T.)
gfOpenTABLE(oAriaApplication.DataDir + "ITEM" , "STYLE",'SH', , .T.)

= lfBuildTmp()

SELECT ITEMADJ
GFSEEK('0002')
SCAN REST WHILE CINVTYPE+STYLE="0002"  FOR   &lcItmExp
WAIT WINDOW "Collecting Data for Item " +STYLE NOWAIT 
  SCATTER MEMVAR
  
  M.FABRIC=IIF(GFSEEK('0002'+STYLE,'ITEM'),ITEM.CSTYMAJOR,'')
  M.FABDESC = ITEM.DESC
  M.COLOR=SUBSTR(STYLE,lnMajor+2,lnNoMajor)
  M.COLDESC =gfCodDes(M.COLOR,'COLOR')
  M.OLDQTY=M.TOTOLD
  M.NMTOTADJ=M.TOTADJ
  M.NOLDTOQTY=M.NTOTOLDTO
  INSERT INTO (lcWorkfile ) FROM MEMVAR
ENDSCAN




*-- if no records matches the selection criteria.
SELECT (lcWorkfile )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN

ENDIF  && end if no records matches the selection criteria.
GOTO TOP
COPY REST TO oAriaApplication.WorkDir + lcTempFile+ ".DBF"  

USE oAriaApplication.WorkDir + lcTempFile+ ".DBF" IN 0 SHARED

SELECT (lcTempFile) &&global function opens DBF (with-optionally- its index) share or exculsive.

*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
*INDEX ON STYLE+cFromWare TAG (lcTempFile)
INDEX ON Fabric+Color+cFromWare TAG (lcTempFile)
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]

=lfAdjustCRSettings()
IF USED(lcTempFile)
    USE IN (lcTempFile)
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
lcOldVal = EVALUATE(OGSYS18())
*-- end of lfWOldVal function .



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
= lFogMover(@laRpSorInv,@laRpTarInv,'Select Inventory Transaction type',.T.,'')  && call mover function.
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
  lcRpInvExp = IIF(EMPTY(lcRpInvExp),LEFT(laTarget[lnI],1),lcRpInvExp + ','+LEFT(laTarget[lnI],1))

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


*!*************************************************************
*! Name      : lfAdjstlcRp
*! Developer : AYM
*! Date      : 11/30/1998
*! Purpose   : ADJUST REPORT EXPRESSION
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAdjstlcRp()
*!*************************************************************

FUNCTION lfAdjstlcRp
*-- lcRpInvExp : Stores value of transaction type.
*-- lcRpexp : passed filter expression from option grid.





*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/30/2004
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]

DIMENSION loOgScroll.laCRParams[4,2]

loOgScroll.lcOGLastForm ='MASTKJL'
loOGScroll.cCROrientation='L'

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTempFile+ ".DBF"

loOgScroll.laCRParams[1,1] = 'OpTitle'
loOgScroll.laCRParams[1,2] = ''

loOgScroll.laCRParams[2,1] = 'ReportName'
loOgScroll.laCRParams[2,2]= 'MATERIAL INVENTORY ADJUSTMENT JOURNAL'   && report title

loOgScroll.laCRParams[3,1] = 'SortBy'
loOgScroll.laCRParams[3,2] = 'SortBy'

loOgScroll.laCRParams[4,1] = 'Layout'
loOgScroll.laCRParams[4,2] = 'Layout'

*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp


DIMENSION laTempStru[13,18] ,laTemplINE[1,18]

STORE '' TO laTempStru,laTemplINE
PRIVATE lnFileCnt , lnFldRow


SELECT ITEMADJ
=OGAFIELDS(@laTemplINE)

laTempStru[1,1] = 'FABRIC'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 19
laTempStru[1,4] = 0

laTempStru[2,1] = 'COLOR'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 6
laTempStru[2,4] = 0

laTempStru[3,1] = 'DYELOT'

laTempStru[4,1] = 'CREASON'
laTempStru[5,1] = 'DATE'
laTempStru[6,1] = 'TYPE'


laTempStru[7,1] = 'CFROMWARE'
laTempStru[8,1] = 'CTOWARE'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 8
  lnFldRow = ASCAN(laTemplINE,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTemplINE,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTemplINE[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTemplINE[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTemplINE[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

laTempStru[9,1] = 'OLDQTY'    
laTempStru[9,2] = 'N'    
laTempStru[9,3] = 10   
laTempStru[9,4] = 2  

laTempStru[10,1] = 'NMTOTADJ' 
laTempStru[10,2] = 'N'  
laTempStru[10,3] = 10
laTempStru[10,4] = 2

laTempStru[11,1] = 'NOLDTOQTY'
laTempStru[11,2] = 'N'
laTempStru[11,3] = 10
laTempStru[11,4] = 2

laTempStru[12 ,1] = 'FABDESC'
laTempStru[12 ,2] = 'C'
laTempStru[12 ,3] = 50
laTempStru[12 ,4] = 0

laTempStru[13 ,1] = 'COLDESC'
laTempStru[13 ,2] = 'C'
laTempStru[13 ,3] = 50
laTempStru[13 ,4] = 0


gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)



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
*! Name      : lfItmPos
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
  

