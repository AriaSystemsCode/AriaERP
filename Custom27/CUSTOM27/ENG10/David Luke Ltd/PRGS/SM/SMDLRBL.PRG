*:***************************************************************************
*: Program file  : SMDLRBL
*: Program desc. : Rebalance program for David Luke 
*: For screen    : SMDLRBL
*:        System : Aria Advantage Series.
*:        Module : System Manager (SM) 
*:     Developer : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SMDLRBL
*:***************************************************************************
*:C123853,1 MHM 01/15/2005
*:***************************************************************************
*: Note that : If we use single warehouse and we find this warehouse
*:           : in warehouse file we must enter screen in view mode. 
*:***************************************************************************
* Modifications
*B130691,1 TMI 12/22/2005 Check if the piktkt is Open then update the LInvoiced flag with .F.
*:***************************************************************************

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF

IF !USED('Pkbinloc')
  =gfOpenFile(gcDataDir+'Pkbinloc','Pkbinloc','SH')
ENDIF

IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ENDIF

*B130691,1  TMI [Start] open the piktkt file
IF !USED('PIKTKT')
  =gfOpenFile(gcDataDir+'PIKTKT','PIKTKT','SH')
ENDIF
*B130691,1  TMI [End  ] 

STORE .F. TO llChckSO,llChckIC

DO (gcScrDir+gcWinAppl+"\SMDLRBL.SPX")

*B130691,1  TMI [Start] Clear the wait window
WAIT CLEAR
*B130691,1  TMI [End  ] 

*!**************************************************************************
*! Name      : lfvReBal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/12/2004
*! Purpose   : Valid rebalance 
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfvReBal

IF llChckIC
  =lfChkWBnLc()
  DIMENSION laQty[9]
  STORE 0 TO laQty
  SELECT WHBINLOC
  SCAN
    WAIT WINDOW 'Update Style ' + STYLE NOWAIT
    STORE 0 TO laQty
    =lfRebalINV()
    SELECT WHBINLOC
    REPLACE QTY1    WITH MAX(laQty[1],0),;
            QTY2    WITH MAX(laQty[2],0),;
            QTY3    WITH MAX(laQty[3],0),;
            QTY4    WITH MAX(laQty[4],0),;
            QTY5    WITH MAX(laQty[5],0),;
            QTY6    WITH MAX(laQty[6],0),;
            QTY7    WITH MAX(laQty[7],0),;
            QTY8    WITH MAX(laQty[8],0),;
            TOTQTY  WITH MAX(laQty[9],0)
  ENDSCAN
  LOCATE
  DELETE ALL FOR TOTQTY = 0
ENDIF

IF llChckSO
  DIMENSION laQty[9]
  STORE 0 TO laQty
  SELECT WHBINLOC
  SCAN
    WAIT WINDOW 'Update Style ' + STYLE NOWAIT
    STORE 0 TO laQty
    =lfRebalALO()
    SELECT WHBINLOC
    REPLACE Alo1    WITH laQty[1],;
            Alo2    WITH laQty[2],;
            Alo3    WITH laQty[3],;
            Alo4    WITH laQty[4],;
            Alo5    WITH laQty[5],;
            Alo6    WITH laQty[6],;
            Alo7    WITH laQty[7],;
            Alo8    WITH laQty[8],;
            TOTAlo  WITH laQty[9]
  ENDSCAN
ENDIF
CLEAR READ

*!**************************************************************************
*! Name      : lfRebalINV
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/12/2004
*! Purpose   : Valid rebalance 
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfRebalINV

SELECT BININVJL
=SEEK(Whbinloc.style+Whbinloc.cwarecode)
SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = Whbinloc.style+Whbinloc.cwarecode
  IF clocation <> Whbinloc.clocation
    LOOP
  ENDIF
  laQty[1] = laQty[1] + nstk1
  laQty[2] = laQty[2] + nstk2
  laQty[3] = laQty[3] + nstk3
  laQty[4] = laQty[4] + nstk4
  laQty[5] = laQty[5] + nstk5
  laQty[6] = laQty[6] + nstk6
  laQty[7] = laQty[7] + nstk7
  laQty[8] = laQty[8] + nstk8
  laQty[9] = laQty[9] + nTotstk
ENDSCAN
*B130691,1  TMI [Start] Get laQty[9] from the actual summed size qty
laQty[9] = laQty[1]+laQty[2]+laQty[3]+laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8]
*B130691,1  TMI [End  ]

*!**************************************************************************
*! Name      : lfRebalALO
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/12/2004
*! Purpose   : Valid rebalance  Alocated
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfRebalALO

PRIVATE LLCHECKORD
SELECT PKBINLOC
= SEEK(WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+WHBINLOC.STYLE)
SCAN REST WHILE CWARECODE+CLOCATION+STYLE=WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+WHBINLOC.STYLE
   *B130691,1  TMI [Start] Be sure that the TotQty field is correct
   REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
   *B130691,1  TMI [End  ] 
   LLCHECKORD = .F.
   *B130691,1  TMI [Start] Check if the piktkt is Open then update the LInvoiced flag with .F.
   IF SEEK(PKBINLOC.PIKTKT,'PIKTKT')
     REPLACE LINVOICED WITH IIF( PIKTKT.STATUS=='O' , .F. , .T. )
   ENDIF 
   *B130691,1  TMI [End  ] 
   IF LINVOICED
      LOOP
   ENDIF
   IF SEEK('O'+ORDER,'ORDLINE')
      SELECT ORDLINE
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)='O'+PKBINLOC.ORDER
         IF PIKTKT <> PKBINLOC.PIKTKT
            LOOP
         ENDIF
         IF STYLE <> PKBINLOC.STYLE
            LOOP
         ENDIF
         LLCHECKORD = .T.
         EXIT
      ENDSCAN
      IF LLCHECKORD
         LAQTY[1] = LAQTY[1]+PKBINLOC.QTY1
         LAQTY[2] = LAQTY[2]+PKBINLOC.QTY2
         LAQTY[3] = LAQTY[3]+PKBINLOC.QTY3
         LAQTY[4] = LAQTY[4]+PKBINLOC.QTY4
         LAQTY[5] = LAQTY[5]+PKBINLOC.QTY5
         LAQTY[6] = LAQTY[6]+PKBINLOC.QTY6
         LAQTY[7] = LAQTY[7]+PKBINLOC.QTY7
         LAQTY[8] = LAQTY[8]+PKBINLOC.QTY8
         LAQTY[9] = LAQTY[9]+PKBINLOC.TOTQTY
      ENDIF
   ENDIF
ENDSCAN
*B130691,1  TMI [Start] Get laQty[9] from the actual summed size qty
laQty[9] = laQty[1]+laQty[2]+laQty[3]+laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8]
*B130691,1  TMI [End  ] 

*!**************************************************************************
*! Name      : lfChkWBnLc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/06/2005
*! Purpose   : Update WHBINLOC for bin location if not found
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
FUNCTION lfChkWBnLc

SELECT BININVJL

SCAN
  *WAIT WINDOW 'Checking Style ' + Style + ' ware house ' + cwarecode + ' bin location ' + clocation NOWAIT  && tmi &&
  WAIT WINDOW 'Checking Style ' + Style + ' Warehouse ' + cwarecode + ' Bin Location ' + clocation NOWAIT
  IF !SEEK(cwarecode+clocation+style,'Whbinloc')
    SELECT Whbinloc
    APPEND BLANK
    REPLACE Style      With BININVJL.Style,;
            cwarecode  With BININVJL.cwarecode,;
            clocation  With BININVJL.clocation
    =gfAdd_Info('WHBINLOC')
  ENDIF
ENDSCAN
