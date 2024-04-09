*:****************************************************************
*: Program file  : HLQUALT.PRG
*: Program desc. : Quality Control.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/22/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvForMat , lfvInvcNo
*:               :            : lfCreatemp , lfCollect
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*B606149,1 ABD Fix Problem that we get new FRX to Print in 2 cases.
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- lcRpInvce :- Variable Hold invoice number.
*-- laRpSource :- Source Array Hold the Format.
*-- laRpTarget :- Target Array Hold the Format.
*-- End Declaration variables.


*- Message Text   :- You must Select a Format for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(laRpTarget)
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must select a format for the report to be displayed.')
  RETURN
ENDIF

*- Message Text   :- You must enter a invoice number for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(ALLTRIM(lcRpInvce) )
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must enter an invoice number for the report to be displayed.')
  RETURN
ENDIF


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()
ENDIF

SELECT (lcTemp)
LOCATE


*-- Print Only the Regulars Invoice
IF ASCAN(laRpTarget,'Regular C & F') > 0 .OR. ;
   ASCAN(laRpTarget,'Regular O.Q.C.')  > 0 
  SELECT (lcTemp)
  SET FILTER TO cInvType = 'R'
  LOCATE 
  IF EOF()
    *- Message Text   :- There is no record to display With format "Regulars"'
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','There is no record to display With format "Regulars"')
  ELSE
    *-- 
    IF ASCAN(laRpTarget,'Regular C & F') > 0
      lcRpForm   = 'HLCFINV'
      lcFormName = 'HLCFINV'
      =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
      = lfRepPltFr(lcFormName)
      SELECT (lcTemp)
      LOCATE
      DO gfDispRe WITH EVAL('lcRpForm')
      
      *B606149,1 ABD - Call new frx . [Begin]
      lcRpForm   = 'HLCFINVI'
      lcFormName = 'HLCFINVI'
      =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
      = lfRepPltFr(lcFormName)
      SELECT (lcTemp)
      LOCATE
      DO gfDispRe WITH EVAL('lcRpForm')
      *B606149,1 ABD - [End]
      
    ENDIF 
    *-- 
    IF ASCAN(laRpTarget,'Regular O.Q.C.')  > 0 
      lcRpForm   = 'HLOQCINV'
      lcFormName = 'HLOQCINV'
      =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
      = lfRepPltFr(lcFormName)
      SELECT (lcTemp)
      LOCATE
      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF 
  ENDIF
  SELECT (lcTemp)
  SET FILTER TO
ENDIF

*-- Print Only the Seconds Invoice
IF ASCAN(laRpTarget,'Seconds C & F') > 0 .OR. ;
   ASCAN(laRpTarget,'Seconds O.Q.C.')  > 0 
  SELECT (lcTemp)
  SET FILTER TO cInvType = 'S'
  LOCATE 
  IF EOF()
    *- Message Text   :- There is no record to display With format "Regulars"'
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','There is no record to display With format "Regulars"')
  ELSE
    *-- 
    IF ASCAN(laRpTarget,'Seconds C & F') > 0
      lcRpForm   = 'HLCFINV'
      lcFormName = 'HLCFINV'
      =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
      = lfRepPltFr(lcFormName)
      SELECT (lcTemp)
      LOCATE
      DO gfDispRe WITH EVAL('lcRpForm')
      
      *B606149,1 ABD - Call new frx . [Begin]
      lcRpForm   = 'HLCFINVI'
      lcFormName = 'HLCFINVI'
      =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
      = lfRepPltFr(lcFormName)
      SELECT (lcTemp)
      LOCATE
      DO gfDispRe WITH EVAL('lcRpForm')
      *B606149,1 ABD - [End]
      
    ENDIF 
    *-- 
    IF ASCAN(laRpTarget,'Seconds O.Q.C.')  > 0 
      lcRpForm   = 'HLOQCINV'
      lcFormName = 'HLOQCINV'
      =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)
      = lfRepPltFr(lcFormName)
      SELECT (lcTemp)
      LOCATE
      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF 
  ENDIF
  SELECT (lcTemp)
  SET FILTER TO
ENDIF


RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/22/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen


DIMENSION laRpSource[4] , laRpTarget[1]
STORE '' TO laRpTarget , laRpSource

laRpSource[1] = 'Regular C & F'
laRpSource[2] = 'Regular O.Q.C.'
laRpSource[3] = 'Seconds C & F'
laRpSource[4] = 'Seconds O.Q.C.'

*-- End OF lfwRepWhen.
*:*************************************************************
*: Name      : lfvForMat
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/22/2002
*: Purpose   : To get the format.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvForMat()
*:*************************************************************
*
FUNCTION lfvForMat

DIMENSION laOldCode [1]
laOldCode = ''
= ACOPY(laRpTarget,laOldCode)
=gfMover(@laRpSource,@laRpTarget,"Format ",.T.,'')

IF ALEN (laOldCode) = ALEN(laRpTarget)
   =ASORT(laOldCode)
   =ASORT(laRpTarget)
  FOR X = 1 TO ALEN (laRpTarget)
    IF laOldCode[X] # laRpTarget[X]
      llOGFltCh = .T.
      EXIT
    ENDIF  
  ENDFOR
ELSE
  llOGFltCh = .T.
ENDIF

*-- End Of lfvForMat
*:*************************************************************
*: Name      : lfvInvcNo
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/22/2002
*: Purpose   : Browse The invoices for the selected vendor,
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvInvcNo ()
*:*************************************************************
*
FUNCTION lfvInvcNo
PRIVATE lnAlias
lnAlias = SELECT (0)

IF !EMPTY (lcRpInvce) .AND. !SEEK(PADR(lcRpInvce,12),'HLCINVHD')
  SELECT HLCINVHD
  DECLARE  laBrowArr[1]
  laBrowArr = ''
  lcBrFields = [ccinvno:H='Invoice Number',cinvtype:H='Invoice Type' ,lcTsuvencd=gfCodDes(CTSUVENCD,'CTSUVENCD'):H='Vendor',dcinvdate:H='Invoice Date',]+;
               [ccinvesel:H='Vessel' , ccinconno:H='Container Number',ccinterms:H='Terms',ncinpackg:H='Packages',]+;
               [ccinshipv:H='Shipped Via' , ncintunit:H='Total Units' , ncintdoz:H='Total Dozens' , ncintval:H='Total Value',]+;
               [namountp:H='Amount Paid']
  
  lcRpInvce   = IIF(ARIABROW('',"CommercialInvoice Header",gnBrFSRow1, gnBrFSCol1,;
                    gnBrFSRow2, gnBrFSCol2,'','','ccinvno','laBrowArr'),;
                    HLCINVHD.ccInvNo,SPACE(12))
ENDIF

SELECT(lnAlias)

*-- End OF lfvInvcNo
*:**************************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Create the temp files.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : None.
*:**************************************************************************
*: Example   : = lfCreatemp()
*:**************************************************************************
*
FUNCTION lfCreatemp

IF !USED(lcTemp)
  CREATE Table (gcWorkDir+lcTemp) (ctsuvencd C(6) , ccinvno C(12)   , cInvType C(1),;
                         dcinvdate D(8) ,ccinvesel C(30)  ,  ccinconno C(30)       ,;
                         ccinterms C(30), ncinpackg N(2)  ,   ccinshipv C(10)      ,;
                         cstynum C(7)    , ccinlotno c(4)                          ,;
                         ncinunits N(6) , ntsudtrat N(12,3), nstyCutCo N(12,3)     ,;
                         cStyCutAt C(6) , nstysewco N(12,3), cStySewAt C (6)       ,;
                         nsetlbfrt N(12,3), nsty_oqc N(12,3), nStyTot  N(12,3)     ,;
                         nsetinfrt N(12,3), nsetfrtso N(12,3),nstyfrtno N(12,3)    ,;
                         nstynetdd N(12,3), nstyprof N(12,3) , StyLot C(11))

ENDIF

SELECT (lcTemp)
ZAP
INDEX ON StyLot  TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')


*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect
*-- lcRpInvce :- Variable Hold invoice number.


SELECT HLCINVHD
IF SEEK(lcRpInvce)
  *-- Scan For the Invoice in header file for the 3 types.
  SCAN REST WHILE ccinvno+ctsuvencd = lcRpInvce
  
    IF SEEK(Hlcinvhd.ctsuvencd+ lcRpInvce,'Hlcinvln')
      SELECT Hlcinvln
      SCAN REST WHILE ctsuvencd+ccinvno+ccvllinno = Hlcinvhd.ctsuvencd + lcRpInvce FOR ;
                      Hlcinvln.cInvType  = Hlcinvhd.cInvType
        = SEEK(cStyNum,'HlcStyle')
        SELECT (lcTemp)
        APPEND BLANK
        REPLACE ctsuvencd WITH Hlcinvhd.ctsuvencd ,;
                ccinvno   WITH Hlcinvhd.ccinvno   ,;
                cInvType  WITH Hlcinvhd.cInvType  ,;
                dcinvdate WITH Hlcinvhd.dcinvdate ,;
                ccinvesel WITH Hlcinvhd.ccinvesel ,;
                ccinconno WITH Hlcinvhd.ccinconno ,;
                ccinterms WITH Hlcinvhd.ccinterms ,;
                ncinpackg WITH Hlcinvhd.ncinpackg ,;
                ccinshipv WITH Hlcinvhd.ccinshipv ,;
                cstynum   WITH Hlcinvln.cstynum   ,;
                ccinlotno WITH Hlcinvln.ccinlotno ,;
                ncinunits WITH Hlcinvln.ncinunits ,;
                ntsudtrat WITH Hlcstyle.ntsudtrat ,;
                nstyCutCo WITH Hlcstyle.nstyCutCo ,;
                cStyCutAt WITH Hlcstyle.cStyCutAt ,;
                nstysewco WITH Hlcstyle.nstysewco ,;
                cStySewAt WITH Hlcstyle.cStySewAt ,;
                nsetlbfrt WITH Hlcstyle.nsetlbfrt ,;
                nsty_oqc  WITH Hlcstyle.nsty_oqc  ,;
                nStyTot   WITH Hlcstyle.nStyTot   ,;
                nsetinfrt WITH Hlcstyle.nsetinfrt ,;
                nsetfrtso WITH Hlcstyle.nsetfrtso ,;
                nstyfrtno WITH Hlcstyle.nstyfrtno ,;
                nstynetdd WITH Hlcstyle.nstynetdd ,;
                nstyprof  WITH Hlcstyle.nstyprof  ,;
                StyLot    WITH (Hlcinvln.cstynum+Hlcinvln.ccinlotno)
        SELECT Hlcinvln
      ENDSCAN
    ENDIF
    SELECT HLCINVHD
  ENDSCAN

ENDIF

*-- End OF lfCollect
*:*************************************************************

