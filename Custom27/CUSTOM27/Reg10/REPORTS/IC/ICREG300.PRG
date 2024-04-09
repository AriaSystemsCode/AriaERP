****************************************************************************
* PROG      : ICREG300.PRG                VERSION : AAS 2.7                 09/02/98
* DATE      : 20/11/2000
* DESC      : Customized style price sheet for CetDuo/Regal.  This report will
*            print only by style as per customer
* Cust      : CestDuo/Regal
* CALL      : ICREG300  TO SET FILE FILTER
****************************************************************************
* Develpoer : ABD ELRAHIM KOURA-(KAM) Due to  C#102037,1          
*
****************************************************************************
*Calls      :functions :lfPrnHdr()
*                      :lfvFabric()
*                      :lfEvalSegs() 
*                      :lfMajPic() 
*                      :lfSetSty() 
****************************************************************************
*--Declartion variables
*--R_TITLE     holld report title
*--R_WIDTH     Wide report
*--PAGENO      hold page number 
*--ROW         hold row number 
*--MAX_LINES   hold maximum line
*--llEnterSacn hold flage for enter scan or not
*--start intializtion variables


R_TITLE = 'STYLE PRICE SHEET'
R_WIDTH     = 'N'                        
PAGENO      = 0                          && Page No.
ROW         = 99                         && Current printing row.
MAX_LINES   = 51   
llEnterSacn =.F. 
IF lcRpDomImp <>  'B'
  lcRpExp = lcRpExp + 'AND' + IIF(lcRpDomImp = 'I' , ' !Make' , ' Make' )
ENDIF
*--start printing
SELECT Style
LOCATE FOR &lcRpExp 
*-- if no record found
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
  SET DEVICE TO SCREEN
ENDIF  

*--Start the report printing.
SET TALK OFF
SET DEVICE TO PRINT
*-- printing data according filter lcRpExp 

SCAN FOR &lcRpExp 
   llEnterSacn =.T.
   WAIT WINDOW STYLE.STYLE NOWAIT
   IF ROW > MAX_LINES
     *--calling header function
     =lfPrnHdr()
   ENDIF
   *--starting detail printing
   ROW = ROW + 1
   @ ROW,00  SAY STYLE
   @ ROW,20 SAY DESC
   @ ROW,45 SAY PRICEA
ENDSCAN
SET DEVICE TO PRINT
*--call procedure for end report
IF !EOF() OR llEnterSacn
  DO ENDREPORT
ELSE
  WAIT WINDOW 'No Record Matching ' NOWAIT
ENDIF  
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name        : lfPrnHdr
*! Developer   : ABD ELRAHIM KOURA (KAM) 
*! Date        : 20/11/2000 
*! Purpose     : print header for report
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : program name,optional titel,wide report
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1

DO RPT_HDR WITH 'ICREG300',lcRpOpTlt,R_WIDTH
ROW = 5
@ ROW,0 SAY 'Style               Description                Wholesale price'
ROW = ROW + 1
@ ROW,0 SAY REPLICATE('*',80)
ROW = ROW + 1
Return

*!*************************************************************
*! Name      : lfvFabric
*! Developer : ABD ELRAHIM KOURA (KAM) 
*! Date      : 20/11/2000
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
*--logical variable if True we close all files open in this function
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
*--lcTag hold the order of fabric file
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABD ELRAHIM KOURA (KAM)
*: Date        : 20/11/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs

STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF

*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 20/11/2000
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic
*--lcmajpic var hold the picture of major part of style
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic


*!*************************************************************
*! Name      : lfSetSty
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 20/11/2000
*! Purpose   : To set or reset the Style browse
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : OpGrdParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetSty ()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm
*--depend on the value of parameter we select the order of index
DO CASE
   CASE OpGrdParm = 'S'
      SET ORDER TO TAG CSTYLE IN STYLE
      GOTO TOP IN Style
   CASE OpGrdParm = 'R'
      SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*********************************************************************************
  