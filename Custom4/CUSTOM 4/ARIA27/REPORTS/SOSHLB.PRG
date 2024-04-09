*:************************************************************************
*: Program file  : SOSHLB.Prg
*: Program desc. : SHIPPING LABELS REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO,AR,AL
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 12/08/98
*: Reference     : *E301076,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfSRVLBL(), lfPikData(), lfInvData(), 
*:                 lfOrdData(), lfvLabels(), lfClearRep()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOSHLB
*:************************************************************************
*: Modifications:
*E301172,1 AMM 03/16/99 Create the graphic version of the label.
*B602967,1 AMM 06/07/99 Enable user to edit number of labels when called from the AR module
*B602807,1 AMM 06/15/99 Fix the bug of variable lnOldVal not found when 
*B602807,1 AMM          pressing ESC in the IN RANGE browse function.
*C101553,1 SSH 06/18/99 1- Add function to call optional program
*C101553,1 SSH          2- Modify the company name to be division long name
*C101553,1 SSH             in case of not empty division long name
*B802491,1 KEN 07/29/99 Fix the bug of variable lfvcanloc not found when pressing 
*B802491,1 KEN          ESC in the select # of labels screen.
*B802499,1 WAB 08/05/99 Print all labels in Preview and print also print number 
*B802499,1              of lable and the max no.
*B802526,1 WAB 08/17/99 Fix the Bug "Variable cOrdType not found" when call the prog 
*B802526,1              From the sales order allocation module.
*B802545,1 WAB 08/23/99 Fix the Bug "you will get a shift problem with diffrent addresses"
*B802570,1 AMM 08/31/99 Fix Bug in multi store orders when printing from alocation module
*B802804,1 AMM 12/07/99 Fix the bug of not printing FORM B (Done in the .lbx)
*B603366,1 MAB 12/24/99 Optimizing data collection when entring OG.
*E301358,1 ARD 12/28/99 Transfer the calculatons of no. of cartons from when
*						function to the validation of the lfSRVLbl function
*B803027,1 SSE 02/13/2000 Fix the bug of printing form addresses,if you have 
*B803027,1                the settings in form codes to print no Addresses.(Done in .lbx)
*C101790,1 SSE 05/24/2000 Add gfDoTriger function in the Valid of In Range 
*C101790,1                screen in case of Invoice for Berkley Shirts 
*C101808,1 WMA 05/24/2000 Convert GMA Shipping Label (ALO40ZY2.LBX) Aria2.6 to Aria2.7
*E301435,1 AME 07/07/2000 Convert C shipping label (ALO40CY2.LBX) Aria2.6 to Aria2.7 
*B803271,1 AME 08/08/2001 Display either the Compnay Name Or Ware Huose Name in Form "A" according to the 
*B803271,1 AME            Shipping label setting from SM module. We modified the Shipping label form (A) Only (.lbx)
*B605440,1 NAD 01/29/2002 Fixing bugs in the realease
*B604925,1 RAE 08/15/2002 Print ORDLINE.CustPO in case of multi PO.
*:************************************************************************
*-- lclblTbl Variable defined by the option grid takes temporary name

*E301358,1 Evaluate that we compute labels before [Begin]
IF RECCOUNT(lclblTbl) = 0
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*E301358,1 Evaluate that we compute labels before [End  ]

*-- Variable to open the customer file again with it.
lcCust_A = gfTempName()
*-- The main temporary file we collect data in it.
LBLTEMP  = gfTempName()
*-- Label form name
*-- Open customer file again to get the Distribution center store addresses 
*-- from , because Customer file is engaged in a relation of the main store.
USE (gcDataDir+'CUSTOMER') AGAIN ALIAS (lcCust_A) IN 0 ORDER TAG CUSTOMER
STORE SPACE(0) TO lcCmAdr1, lcCmAdr2, lcCmAdr3, lcCmAdr4, lcAddr1 ,lcAddr2,;
                  lcAddr3,lcActNam, lcAcct, lcMainF
*-- Get the setting of print Company/warehouse addresses.

lcPrtAdr = IIF(TYPE('lcPrtAdr') = 'C', lcPrtAdr, 'C')
IF lcPrtAdr = 'C'
  =SEEK(gcAct_Comp,'SYCCOMP')
  lcCompName = SYCCOMP.cCom_Name
  
  *-- Get the company addresses
  lcCmAdr1 = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  lcCmAdr2 = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  lcCmAdr3 = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  lcCmAdr4 = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  lcCmAdr5 = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
ENDIF


DO CASE
  CASE lcXTYPE = 'P'
    SELECT PIKTKT
    =AFIELDS(laFileStru)
    IF ASCAN(laFileStru,'LABELS') = 0
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,4]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'LABELS'
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
    ENDIF

    *B802499,1 WAB - Add field to temp. file struc. to hold No. of lable
    *B802499,1 WAB - START

    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'nLblsNo'
    laFileStru[lnFileStru,2] = 'N'
    laFileStru[lnFileStru,3] = 4
    laFileStru[lnFileStru,4] = 0

    *B802499,1 WAB - END

    CREATE TABLE (gcWorkDir+LBLTEMP) FROM ARRAY laFileStru
    INDEX ON PIKTKT TAG &LBLTEMP

    IF !lfpikData()
      RETURN
    ENDIF

  CASE lcXTYPE = 'I'
    SELECT INVHDR
    =AFIELDS(laFileStru)
    IF ASCAN(laFileStru,'LABELS') = 0
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,4]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'LABELS'
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
    ENDIF

    *B802499,1 WAB - Add field to temp. file struc. to hold No. of lable
    *B802499,1 WAB - START

    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'nLblsNo'
    laFileStru[lnFileStru,2] = 'N'
    laFileStru[lnFileStru,3] = 4
    laFileStru[lnFileStru,4] = 0

    *B802499,1 WAB - END

    CREATE TABLE (gcWorkDir+LBLTEMP) FROM ARRAY laFileStru
    INDEX ON INVOICE+STORE TAG &LBLTEMP
    
    IF !lfInvData()
      RETURN
    ENDIF
  CASE lcXTYPE = 'O'
    SELECT ORDHDR
    =AFIELDS(laFileStru)
    IF ASCAN(laFileStru,'LABELS') = 0    
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,4]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'LABELS'
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
    ENDIF

    *B802499,1 WAB - Add field to temp. file struc. to hold No. of lable
    *B802499,1 WAB - START

    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'nLblsNo'
    laFileStru[lnFileStru,2] = 'N'
    laFileStru[lnFileStru,3] = 4
    laFileStru[lnFileStru,4] = 0
      
    *B802499,1 WAB - END

    CREATE TABLE (gcWorkDir+LBLTEMP) FROM ARRAY laFileStru
    INDEX ON ORDER+STORE TAG &LBLTEMP
    *-- Fill the temprary file with report data
    IF !lfOrdData()
      RETURN
    ENDIF
ENDCASE
*E301172,1 AMM Comment out, no need to it
*SET DEVICE TO PRINT
*R_WIDTH = 'N'
*E301172,1 AMM end

SELECT (LBLTEMP)
SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
        'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER

*E301172,1 AMM Change the printing way.
*SCAN FOR !EOF('ORDHDR') .AND. LABELS # 0
  *lnLabels = IIF(gcDevice = "SCREEN",1,&LBLTEMP..LABELS)
  *-- First line : Account XXXXX STORE : XXXXXX
  *lcAcct   = OrdHdr.Account+IIF(EMPTY(STORE),'',' Store: '+Store)
  *-- Get Name and addresses
  *=lfGetAddr()
  *WAIT WINDOW 'Printing '+IIF(lcXType='O','order '+ORDHDR.Order,IIF(lcXType='I','invoice '+Invoice,'pick ticket '+PIKTKT)) NOWAIT
  *-- Print Required number of labels
  *FOR lnc=1 TO lnLabels
     *LABEL FORM (GCREPHOME+lcFormName) NEXT 1 TO PRINT NOCONSOLE
  *ENDFOR
*ENDSCAN
*E301172,1 AMM If printing to screen, (LBLTEMP) is the main printing file, 
*E301172,1 AMM else Create new table and duplicate its records to the number 
*E301172,1 AMM of labels.
SELECT (LBLTEMP)

*B802499,1 WAB - When gcDevice is 'screen' Print like gcDevice is 'Printer'
*IF gcDevice # "SCREEN"
*B802499,1 WAB - END
*E301172,1 AMM Create a new temporary table and insert records equal to numbre of labels
lcMainF = gfTempName()
COPY STRUCTURE TO (gcWorkDir+lcMainF) WITH CDX
USE (gcWorkDir+lcMainF) IN 0 ORDER 1
SCAN FOR !EOF('ORDHDR') .AND. LABELS # 0
  WAIT WINDOW 'Printing '+IIF(lcXType='O','order '+ORDHDR.Order,IIF(lcXType='I','invoice '+Invoice,'pick ticket '+PIKTKT)) NOWAIT
  *-- Print Required number of labels
     
  FOR lnc=1 TO &LBLTEMP..LABELS
    SCATTER MEMVAR
    *B802499,1 WAB - store the print label no to m.nlblsno
    *B802499,1 WAB - START

    m.nLblsNo = lnc
    *B802499,1 WAB - END

    INSERT INTO (lcMainF) FROM MEMVAR
  ENDFOR
ENDSCAN
SET RELATION TO
SELECT (lcMainF)
SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
      'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
*ELSE
*  SET FILTER TO LABELS # 0

*B802499,1 WAB - No need for endif cause we comment out the If statment
*B802499,1 WAB - START
*ENDIF
*B802499,1 WAB - END

*E301172,1 AMM Print by GFDISPRE()

*DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
*SET DEVICE TO SCREEN
*gcDevice = 'SCREEN'

*C101553,1 SSH 06/18/99 1- Add function to call optional program.
lcCompName = ''

=lfOptProg()

*C101553,1 SSH (END)
*ahmed
*DO gfDispRe WITH lcFormName,'LABELS # 0',.F.,'L' 

=gfDispRe(lcFormName,'LABELS # 0',.F.,'L')
*ahmed end
*E301172,1 AMM end

IF USED(lcCust_A)
  USE IN (lcCust_A)
ENDIF
*E301172,1 AMM Close tables and erase temproray files
IF USED(lcMainF)
  USE IN (lcMainF)
ENDIF
ERASE (gcWorkDir+lcMainF+'.DBF')
ERASE (gcWorkDir+lcMainF+'.CDX')
IF USED(LBLTEMP)
  USE IN (LBLTEMP)
ENDIF
ERASE (gcWorkDir+LBLTEMP+'.DBF')
ERASE (gcWorkDir+LBLTEMP+'.CDX')
*E301172,1 AMM end

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
*-- Variable to execute the function one time just when entering the 
*-- option grid

*B605440,1 NAD
*IF !llWhen
*  llWhen = .T.
*ELSE
*  RETURN
*ENDIF
*B605440,1 NAD
lclblTbl  = gfTempName()

*E301358,1 Comment out most of the function avoiding data collection when entring OG. [Begin]
*lcTmpOrdL = gfTempName()
*lcTmpSTY  = gfTempName()
DO CASE
  *-- Current module is Sales Order
  CASE GCACT_APPL='SO'
    lcXType = 'O'
    CREATE TABLE (gcWorkDir+lclblTbl) (ORDER C(6),LABELS N(4))
    INDEX ON ORDER TAG (lclblTbl)
    SET ORDER TO ORDLINE IN ORDLINE
    SET ORDER TO STYLE IN STYLE

    *-- Set order to .
    SET ORDER TO ORDHDR IN ORDHDR
        
    *--E301358,1 Comment Out Collection block [Begin]
    *SELECT ORDLINE
    *SET RELATION TO STYLE INTO STYLE ADDITIVE

    *SELECT ORDHDR
    *SET ORDER TO ORDHDR
    *SET RELATION TO 'O'+ORDER INTO ORDLINE ADDITIVE
    *=SEEK('O')
    
    *-- Calculate number of labels required to the order.
    *SCAN WHILE cOrdType = 'O' FOR STATUS # 'X'
    *  SELECT ORDLINE
    *  lnLab = 0
    *  SCAN WHILE cOrdType+Order = 'O'+OrdHdr.Order 
    *    lnLab = lnLab + IIF(STYLE.Qty_Ctn=0,1,CEILING(OrdLine.TotQty/STYLE.Qty_Ctn))
    *  ENDSCAN
    *  INSERT INTO (lclblTbl) (Order, Labels) VALUES (OrdHdr.Order,lnLab)
    *ENDSCAN
    *SET RELATION TO
    *SELECT ORDLINE
    *SET RELATION TO
    *--E,301358,1 Comment Out Collection block [End  ]
  
  *-- Current module is Account Receivable
  CASE GCACT_APPL='AR'
    lcXType = 'I'
    CREATE TABLE (gcWorkDir+lclblTbl) (INVOICE C(6),LABELS N(4))
    INDEX ON INVOICE TAG (lclblTbl)

    *C101790,1 SSE calling gfDoTriger on condition [Begin]
    =gfDoTriger('SOSHLB',PADR('CREATEFILE',10))
    *C101790,1 SSE [End]

    *E301358,1
    SET ORDER TO INVHDR IN INVHDR
        
    *E301358,1 Comment Out Collection block [Begin]
    *SELECT INVHDR
    *SCAN FOR STATUS # 'V'
    *  INSERT INTO (lclblTbl) (INVOICE) VALUES (INVHDR.INVOICE)
    *ENDSCAN
    *--E301358,1 Comment Out Collection block [End  ]

  *-- Current module is Allocation
  CASE GCACT_APPL='AL'
    lcXType = 'P'
    CREATE TABLE (gcWorkDir+lclblTbl) (PIKTKT C(6),ORDER C(6),LABELS N(4))
    INDEX ON PIKTKT TAG (lclblTbl)
    SET ORDER TO ORDLINE IN ORDLINE
    SET ORDER TO PIKLINE IN PIKLINE
    SET ORDER TO STYLE   IN STYLE

    *--E301358,1 Comment Out Collection block [Begin]
    *USE (gcDataDir+'STYLE') AGAIN ALIAS (lcTmpSTY) IN 0 ORDER TAG STYLE
    *USE (gcDataDir+'ORDLINE') AGAIN ALIAS (lcTmpOrdL) IN 0 ORDER TAG ORDLINE

    *B603366,1 Seek instead of too many relations and No do case [Begin]
    *SELECT (lcTmpOrdL)
    *SET RELATION TO STYLE INTO (lcTmpSTY)
    *SELECT ORDLINE
    *SET RELATION TO STYLE INTO STYLE
    *SELECT PIKLINE
    *SET RELATION TO 'O'+ORDER+STR(LINENO,6) INTO ORDLINE ADDITIVE
    *SELECT PIKTKT
    *SET RELATION TO PIKTKT INTO PIKLINE ADDITIVE
    *SET RELATION TO 'O'+ORDER INTO (lcTmpOrdL) ADDITIVE
    *B603366,1 Seek instead of too many relations and No do case [End  ]

    *SCAN FOR STATUS # 'X'
    *  lnLab = 0

      *B603366,1 new code instead of do case block [Begin]
      *DO CASE
        *-- If the status is complete or canceled , the data is in PIKLINE
      *  CASE STATUS = 'C'
      *    IF !EOF('PIKLINE') .AND. !EOF('ORDLINE') .AND. !EOF('STYLE')

      *      SELECT PIKLINE

            *B802526,1 WAB  - THERE IS NO FILED WITH NAME cOrdType In PIKLINE TABLE             
            *B802526,1 WAB  - START
            *SCAN WHILE cOrdType+ORDER = 'O'+PIKTKT.ORDER

      *      SCAN WHILE ORDER = ORDLINE.ORDER

            *B802526,1 WAB  - END

      *        lnLab = lnLab + IIF(STYLE.Qty_Ctn = 0, 0 ,CEILING(PIKLINE.TotQty/STYLE.Qty_Ctn))
      *      ENDSCAN
      *    ENDIF
        *-- If status is Open or Hold, the data is stored in ORDLINE
      *  CASE STATUS $ 'OH'
          
      *    IF !EOF(lcTmpOrdL) .AND. !EOF(lcTmpSty)
      *      SELECT (lcTmpOrdL) 
            *B802570,1 AMM Loop only on the required pick ticket
            *SCAN WHILE cOrdType+ORDER = 'O'+PIKTKT.ORDER
      *      SCAN WHILE cOrdType+ORDER = 'O'+PIKTKT.ORDER FOR &lcTmpOrdL..PICKED .AND. &lcTmpOrdL..PIKTKT = PIKTKT.PIKTKT
            *B802570,1 AMM end
      *        lnLab = lnLab + IIF(&lcTmpSty..Qty_Ctn = 0, 0 ,CEILING(&lcTmpOrdL..TotPik/&lcTmpSty..Qty_Ctn))
      *      ENDSCAN
      *    ENDIF
      *ENDCASE
      
      *-- If the status is complete or canceled , the data is in PIKLINE
      *IF STATUS = 'C' AND ;
      *   SEEK(PIKTKT,"PIKLINE") AND ;
      *   SEEK("O"+ORDER,"ORDLINE") AND ;
      *   SEEK(ORDLINE.STYLE,"STYLE")

      *  SELECT PIKLINE
      *  SCAN REST WHILE piktkt+order+STR(lineno,6) = PikTkt.PikTkt + PikTkt.ORDER
      *    lnLab = lnLab + IIF(STYLE.Qty_Ctn = 0, 0 ,CEILING(PIKLINE.TotQty/STYLE.Qty_Ctn))
      *  ENDSCAN
      
      *ENDIF   

      *SELECT PIKTKT
      *-- If status is Open or Hold, the data is stored in ORDLINE
      *IF STATUS $ 'OH' AND ;
      *   SEEK("O"+ORDER,lcTmpOrdL) AND ;
      *   SEEK(&lcTmpOrdL..Style,lcTmpSty)

      *  SELECT (lcTmpOrdL)
      *  SCAN REST WHILE cOrdType+ORDER+STR(LineNo,6) = 'O'+PIKTKT.ORDER
      *    IF &lcTmpOrdL..PICKED .AND. &lcTmpOrdL..PIKTKT = PIKTKT.PIKTKT
      *      lnLab = lnLab + IIF(&lcTmpSty..Qty_Ctn = 0, 0 ,CEILING(&lcTmpOrdL..TotPik/&lcTmpSty..Qty_Ctn))
      *    ENDIF
      *  ENDSCAN  
      *ENDIF
      *B603366,1 new code instead of do case block [End  ]

      *IF !EMPTY(PIKTKT.PIKTKT)
      *  INSERT INTO (lclblTbl) (PIKTKT,Order,LABELS) VALUES ;
      *                       (PIKTKT.PIKTKT,PIKTKT.ORDER,lnLab)
      *ENDIF
    
    *ENDSCAN
    
    *B603366,1 Seek instead of too many relations and No do case [Begin ]
    *SELECT PIKTKT
    *SET RELATION TO
    *SELECT PIKLINE
    *SET RELATION TO
    *SELECT ORDLINE
    *SET RELATION TO 
    *IF USED(lcTmpOrdL)
    *  USE IN (lcTmpOrdL)
    *ENDIF
    *IF USED(lcTmpSTY)
    *  USE IN (lcTmpSTY)
    *ENDIF
    *B603366,1 Seek instead of too many relations and No do case [End  ]
    *--E,301358,1 Comment Out Collection block [End  ]

ENDCASE
*E,301358,1 Comment out most of the function avoiding data collection when entring OG. [End  ]

*-- end of lfwOGWhen.

*!*************************************************************
*! Name      : lfSRVLBL
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : control browsing (Order , Invoice or PikTkt) and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVLBL()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVLBL
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    DO CASE 
      CASE lcxType = 'O'
        
        SELECT ORDHDR
        *-- Set relation to that temporary file to get the default number of labels
        SET RELATION TO ORDER INTO (lclblTbl)
        
        *-- Set relation to customer to get the account address in the INRANGE browse
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE

      CASE lcxType = 'I'
        SELECT INVHDR
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE
        
        SET RELATION TO INVOICE INTO (lclblTbl) ADDITIVE
        
      CASE lcxType = 'P'
        SET ORDER TO TAG CUSTOMER IN CUSTOMER
        SELECT PIKTKT
        SET RELATION TO 'O' + Order INTO ORDHDR
        SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                        'S' + Account + Store) INTO CUSTOMER ADDITIVE
        SET RELATION TO PIKTKT INTO (lclblTbl) ADDITIVE
    ENDCASE
    
  CASE lcParm = 'R'  && Reset code
    DO CASE 
      CASE lcxType = 'O'
        SELECT ORDHDR
        SET RELATION TO
      CASE lcxType = 'I'
        SELECT INVHDR
        SET RELATION TO
      CASE lcxType = 'P'
        SELECT PIKTKT
        SET RELATION TO 
    ENDCASE

  OTHERWISE      && Valid code

    *E301358,1
    =!lfLblFound() AND lfNoOfLbls()

    DO CASE 
      CASE (lcxType = 'O' .OR. lcxType = 'P' ) .AND. !cbdspdlg
        lnLabels = EVAL(lclblTbl+'.Labels')
        lnOldVal = lnLabels
        PUSH KEY
        ON KEY
        *-- Call the screen to edit the number of labels
        DO (GCREPHOME+'SOSHLB.SPR') WITH ;
        IIF(lcxType = 'O' ,'Order# ' + ORDHDR.Order +' Labels',;
              'Pick Ticket ' + PIKTKT.PIKTKT)
        POP KEY
        IF lnLabels # lnOldVal
          lnCurAls = SELECT(0)
          lcKey = KEY()
          SELECT (lclblTbl)
          *-- Update table with the new number of labels entered by user.
          REPLACE LABELS WITH lnLabels
          SELECT (lnCurAls)
          =SEEK(lcKey)            
       ENDIF
        
      CASE lcxType = 'I'
        
        *-- Calculate required number of tables, however don't allow user to edit.
        SET ORDER TO CONSINVH IN CONSINVH
        lnCurAls = SELECT(0)
        lcKey = KEY()
        lcExpr = INVHDR.INVOICE
        IF INVHDR.CONSOL # 'Y'
          SELECT (lclblTbl)
          REPLACE LABELS WITH INVHDR.CARTONS
          *B602967,1 AMM Get number of labels
          lnLab = INVHDR.CARTONS
          *B602967,1 AMM end
        ELSE

          *-- If consolidated invoice, calculate number of labels from CONSINVH
          SELECT CONSINVH
          IF SEEK(lcExpr,'CONSINVH')
            lnLab = 0
            SCAN WHILE INVOICE = INVHDR.INVOICE
                lnLab = lnLab + CONSINVH.CARTONS
            ENDSCAN
          ELSE
            lnLab = INVHDR.CARTONS
          ENDIF
          SELECT (lclblTbl)
          IF SEEK(lcExpr)
            REPLACE LABELS WITH lnLab
          ENDIF
        ENDIF
        *B602967,1 AMM Call edit labels number screen
        lnLabels = lnLab
        lnOldVal = lnLab
        PUSH KEY
        ON KEY

        *-- Call the screen to edit the number of labels
        *C101790,1 SSE Add gfDoTriger to display Customized screen 
        *C101790,1     for Berkley Shirts [Begin] 
        IF gfDoTriger('SOSHLB',PADR('LABELSINFO',10))
        ELSE
          DO (gcRepHome+'SOSHLB.SPR') WITH ;
                    'Invoice# ' + InvHdr.invoice +' Labels'
        ENDIF
        *C101790,1 SSE [End]

        POP KEY
        IF lnLabels # lnOldVal
          SELECT (lclblTbl)
          *-- Update table with the new number of labels entered by user.
          REPLACE LABELS WITH lnLabels
        ENDIF
        *B602967,1 AMM end
        
        SELECT (lnCurAls)
        =SEEK(lcKey)
    ENDCASE
ENDCASE

*!*************************************************************
*! Name      : lfPikData
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : Collect the PikTkt data for the report
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : SOSHLB.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPikData()
*!*************************************************************
FUNCTION lfPikData
*-- Don't collect PIKTKTs with status "Canceled"
lcrpexp = lcrpexp + IIF(EMPTY(lcrpexp),'',' .AND. ') + "PIKTKT.STATUS # 'X'"
SELECT PIKTKT
SET RELATION TO PIKTKT INTO (lclblTbl) ADDITIVE

*E301358,1 comment out this block [Begin]
*LOCATE ALL FOR &lcrpexp .AND. &lclblTbl..LABELS > 0
*IF EOF()
*  *-- Message 'There are no records to display...!'
*  =gfModalGen('TRM00052B00000','DIALOG')
*  RETURN .F.
*ENDIF

*SCAN FOR &lcRpExp
*E301358,1 comment out this block [end  ]

*E301358,1 Scaning for the Label No.
SCAN FOR &lcRpExp .AND. &lclblTbl..LABELS > 0
  WAIT WINDOW "Collecting Data for Pick Ticket # " + PikTkt NOWAIT
  SCATTER MEMVAR
  M.LABELS = &lclblTbl..LABELS
  INSERT INTO (LBLTEMP) FROM MEMVAR
ENDSCAN

IF RECCOUNT(LBLTEMP) = 0
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
*-- end of lfPikData.

*!*************************************************************
*! Name      : lfInvData
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : Collect the invoice data for the report
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfInvData()
*!*************************************************************
FUNCTION lfInvData

*-- Don't collect voided invoices
lcrpexp = lcrpexp + IIF(EMPTY(lcrpexp),'',' .AND. ') + "INVHDR.STATUS # 'V'"

SET ORDER TO Ordhdr IN ORDHDR
SELECT INVHDR
SET RELATION TO INVOICE INTO (lclblTbl) ADDITIVE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE

LOCATE ALL FOR &lcrpexp .AND. !EOF('ORDHDR')
IF EOF()

  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
SCAN FOR &lcrpexp .AND. !EOF('ORDHDR')
  *ARD Wait window
  WAIT WINDOW "Collecting Data for Invoice # " + Invoice NOWAIT

  SCATTER MEMVAR
  *-- In consolidated invoice, Add record with the required labels for 
  *-- each store
  IF INVHDR.CONSOL = 'Y' .AND. SEEK(INVHDR.INVOICE,'CONSINVH')
    SELECT CONSINVH
    m.Store = CONSINVH.STORE    
    lnLab = 0
    SCAN WHILE INVOICE = INVHDR.INVOICE
      IF m.STORE # CONSINVH.STORE
        m.LabELS = lnLab
        INSERT INTO (LBLTEMP) FROM MEMVAR      
        m.Store  = CONSINVH.Store
        lnLab    = CONSINVH.CARTONS
      ELSE
        lnLab = lnLab + CONSINVH.CARTONS
      ENDIF
    ENDSCAN
    m.Labels = lnLab
  ELSE
    M.LABELS = &lclblTbl..LABELS
  ENDIF
  IF M.LABELS > 0
    INSERT INTO (LBLTEMP) FROM MEMVAR
  ENDIF
ENDSCAN
SET RELATION TO
IF EOF(LBLTEMP)

  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfOrdData
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : Collect Order data for the report
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOrdData()
*!*************************************************************
FUNCTION lfOrdData

*-- Don't collect orders with status "CANCELED"
lcrpexp = lcrpexp + IIF(EMPTY(lcrpexp),'',' .AND. ') + "ORDHDR.STATUS # 'X'"
IF !EOF(LBLTEMP)
  SELECT LBLTEMP
  ZAP
ENDIF
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDHDR
*-- Set relation to that temporary file to get the default number of labels
SET RELATION TO ORDER INTO (lclblTbl)

*E3011358,1 Change the logic exp. for FOR
*LOCATE ALL FOR &lcrpexp
LOCATE ALL FOR &lcrpexp .AND. &lclblTbl..LABELS > 0

IF EOF()

  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF

*E301358,1 changing the SCAN condition 
*SCAN FOR &lcrpexp
SCAN FOR &lcrpexp .AND. &lclblTbl..LABELS > 0

  *ARD WAIT WINDOW
  WAIT WINDOW "Collecting Data for Order # " + Order NOWAIT

  SCATTER MEMVAR
  M.LABELS = &lclblTbl..LABELS
  IF OrdHdr.Multi # 'Y'
    INSERT INTO (LBLTEMP) FROM MEMVAR
  ELSE
    *-- If multi store order, insert a record in LBLTEMP file for each 
    *-- store to print required labels for each store
    SELECT ORDLINE

    *B802499,1 WAB - the index key is the order type + order no 
    *B802499,1 WAB - START
    *IF SEEK(ORDHDR.ORDER)

    IF SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER)
      *B604925,1 RAE Print ORDLINE.CustPO in case of multi PO. [Start]
      m.CustPO = ORDLINE.CustPO
      *B604925,1 RAE [End]
    
      *B802499,1 WAB - END
      M.Store = ORDLINE.STORE
      lnLab = 0
      SCAN WHILE ORDER = ORDHDR.ORDER
        IF m.STORE # ORDLINE.STORE 
          m.Labels = lnLab
          INSERT INTO (LBLTEMP) FROM MEMVAR
          m.STORE = ORDLINE.STORE 

          *B604925,1 RAE Print ORDLINE.CustPO in case of multi PO. [Start]
          m.CustPO = ORDLINE.CustPO
          *B604925,1 RAE [End]

          lnLab = IIF(STYLE.Qty_Ctn=0,1,CEILING(OrdLine.TotQty/STYLE.Qty_Ctn))
        ELSE
          lnLab = lnLab + IIF(STYLE.Qty_Ctn=0,1,CEILING(OrdLine.TotQty/STYLE.Qty_Ctn))
        ENDIF
      ENDSCAN
      m.Labels = lnLab
      INSERT INTO (LBLTEMP) FROM MEMVAR
    ENDIF
  ENDIF
ENDSCAN
SET RELATION TO

SELECT ORDLINE
SET RELATION TO


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : Closing files upon exitting the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep


IF USED(lcTmpOrdL)
  USE IN (lcTmpOrdL)
ENDIF
IF USED(lclblTbl)
  USE IN (lclblTbl)
ENDIF
IF USED(lcTmpSTY)
  USE IN (lcTmpSTY)
ENDIF
*E301172,1 AMM Close tables and erase temproray files
ERASE (gcWorkDir+lclblTbl+'.DBF')
ERASE (gcWorkDir+lclblTbl+'.CDX')
*E301172,1 AMM end

SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : Valid function of the Cancel button of the edit 
*!             number of labels screen
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : SOSHLB.SPR
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCancel()
*!*************************************************************
*B602807,1 AMM Change the function name, to differentriate it from 
*B602807,1 AMM the cancel valid function of in range browse
*FUNCTION lfvCancel
*B802491,1 KEN 
*FUNCTION lfvCanLab
FUNCTION lfvCanLoc

lnLabels = lnOldVal

*!*************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/08/98
*! Purpose   : Get warehouse and ship to addresses
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : SOSHLB.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetAddr()
*!*************************************************************
FUNCTION lfGetAddr

lnAlias = SELECT(0)
lcAcct   = OrdHdr.Account+IIF(EMPTY(STORE),'',' Store: '+Store)
*C101553,1 SSH          2- Modify the company name to be division long name
*C101553,1 SSH             in case of not empty division long name
lcDivLName = ''
DIMENSION laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
* amin
*lcCompName = IIF(!EMPTY(lcDivLName),lcDivLName,SUBSTR(gcCompName,1,29))
lcCompName = IIF(!EMPTY(lcDivLName),lcDivLName,SUBSTR(gcCom_Name,1,29))
* amin
*C101553,1 SSH(END)

*C101808,1 WMA [Begin]
=SEEK(gcAct_Comp,'SYCCOMP')
*C101808,1 WMA [End]

IF ORDHDR.ALT_SHPTO
  lcActNam = ORDHDR.STNAME
  *B602807,1 AMM start 
  *lcAddr1  = cAddress1
  *lcAddr2  = cAddress2
  *lcAddr3  = cAddress3
  lcAddr1  = ORDHDR.cAddress1
  lcAddr2  = ORDHDR.cAddress2
  lcAddr3  = ORDHDR.cAddress3
  *B602807,1 end

ELSE
  IF !EMPTY(CUSTOMER.DIST_CTR) .AND. SEEK('S'+CUSTOMER.ACCOUNT+CUSTOMER.DIST_CTR,lcCust_A)
    lcActNam = IIF(EMPTY(&lcCust_A..DBA),&lcCust_A..STNAME,&lcCust_A..DBA)
  ELSE
    lcActNam = IIF(EMPTY(CUSTOMER.DBA),CUSTOMER.STNAME,CUSTOMER.DBA)    
  ENDIF
  IF EMPTY(CUSTOMER.DIST_CTR)
    SELECT CUSTOMER
    lcAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    lcAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    lcAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  ELSE
    SELECT (lcCust_A)
    lcAddr1 = gfGetAdr(lcCust_A,'' , '' , '' , 1)
    lcAddr2 = gfGetAdr(lcCust_A,'' , '' , '' , 2)
    lcAddr3 = gfGetAdr(lcCust_A,'' , '' , '' , 3)
  ENDIF
ENDIF

*B802545,1 WAB- declare array hold the 3 address and shifting betwwen then if
*B802545,1 WAB- if There are any empty filed 
*B802545,1 WAB- START
DECLARE lasort[3]
FOR lnI = 1 TO 3
  lcI = ALLTRIM(STR(lnI))
  lasort[lnI] = lcAddr&lcI
ENDFOR
FOR lnI = 1 TO 2
  FOR lnC = 1 to 2
    IF Empty(laSort[lnC])
      laSort[lnC] = laSort[lnC+1]
      laSort[lnC+1] = ' '
    ENDIF
  ENDFOR
ENDFOR
FOR lnI = 1 TO 3
  lcI = ALLTRIM(STR(lnI))
  lcAddr&lcI =  lasort[lnI] 
ENDFOR
*B802545,1 WAB- END

IF lcPrtAdr = 'W'
  DO CASE
    CASE lcXTYPE = 'P' .OR. lcXTYPE = 'I'
    
       *B802499,1 WAB - the file who display from it is (lcmainf)
       *B802499,1 WAB -START
       *=SEEK(&LBLTEMP..cWareCode,'WAREHOUS')
    
       =SEEK(&lcMainF..cWareCode,'WAREHOUS')
    
       *B802499,1 WAB - END
    
    CASE lcXTYPE = 'O'
      =SEEK(OrdHdr.cWareCode,'WAREHOUS')
  ENDCASE
  *-- Get the company addresses
  lcCmAdr1 = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  lcCmAdr2 = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  lcCmAdr3 = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  lcCmAdr4 = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  lcCmAdr5 = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF
SELECT (lnAlias)
RETURN ''

*!*************************************************************
*! Name      : lfNoOfLbls
*! Developer : Ahmed Reda ARD (E301358,1)
*! Date      : 12/28/99
*! Purpose   : Calculate Number of labels in all cases [AR-AL-SO]
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!E301358,1
*!
FUNCTION lfNoOfLbls
PRIVATE lnCurrAls , lnNoOfLbls
lnNoOfLbls = 0
lnCurrAls = SELECT(0)  && Save Active Alias

DO CASE
  *-- Current module is Sales Order
  CASE gcAct_Appl='SO'
    *OrdHdr.cOrdType+OrdHdr.Order 
    IF SEEK(OrdHdr.cOrdType+OrdHdr.Order,"ORDLINE")
      SELECT ORDLINE
      SCAN REST WHILE cordtype+order+STR(lineno,6) = OrdHdr.cOrdType+OrdHdr.Order
        lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
      ENDSCAN
    ENDIF
    INSERT INTO (lclblTbl) (Order, Labels) VALUES (OrdHdr.Order,lnNoOfLbls)

  *-- Current module is Account Receivable
  CASE gcAct_Appl='AR'
    *Invoice
    IF InvHdr.STATUS # 'V'
      INSERT INTO (lclblTbl) (INVOICE) VALUES (INVHDR.INVOICE)
    ENDIF

  *-- Current module is Allocation
  CASE gcAct_Appl='AL'
    *-- if PikTkt is not cancelled.
    IF PIKTKT.STATUS # "X"

      IF PIKTKT.STATUS = "C" AND SEEK(PIKTKT.PIKTKT,"PIKLINE")
         lcScanCond  = PikTkt.PikTkt + PikTkt.Order
         
         SELECT PIKLINE
         SCAN REST WHILE piktkt+order+STR(lineno,6) = lcScanCond
           lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
         ENDSCAN
      ENDIF
      
      IF PIKTKT.STATUS $ 'OH' AND SEEK("O"+PikTkt.Order,"ORDLINE")
        SELECT ORDLINE
        SCAN REST WHILE cOrdType+ORDER+STR(LineNo,6) = 'O'+PIKTKT.ORDER
          IF PICKED AND PIKTKT = PIKTKT.PIKTKT
            lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
          ENDIF
        ENDSCAN  
      ENDIF
    
    ENDIF  && end if PikTkt is not cancelled.
    INSERT INTO (lclblTbl) (PIKTKT,Order,LABELS) VALUES ;
                           (PIKTKT.PIKTKT,PIKTKT.ORDER,lnNoOfLbls)
    
ENDCASE

SELECT (lnCurrAls)
*-- end of lfNoOfLbls.

*!*************************************************************
*! Name      : lfCalcrtns
*! Developer : Ahmed Reda ARD (E301358,1)
*! Date      : 12/28/99
*! Purpose   : General formula to Calculate Number of Cartons.
*!*************************************************************
*! Return    : No. Of Cartons.
*!*************************************************************
*E301358,1
*!
FUNCTION lfCalcrtns
PARAMETERS lnCalcrtns
PRIVATE lnCalcrtns
=SEEK(Style,"STYLE")
RETURN lnCalcrtns + IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*-- end of lfCalcrtns.

*!*************************************************************
*! Name      : lfLblFound
*! Developer : Ahmed Reda ARD (E301358,1)
*! Date      : 12/28/99
*! Purpose   : Check the Existance Of the No. Of Label In the temp File.
*!*************************************************************
*! Return    : .T. if label found else .F.
*!*************************************************************
*E301358,1
*!
FUNCTION lfLblFound
PRIVATE lcToSeek
lcToSeek = IIF(gcAct_Appl='SO',[ORDHDR.ORDER],IIF(gcAct_Appl='AR',[INVHDR.INVOICE],;
                               [PIKTKT.PIKTKT]))
RETURN SEEK(&lcToSeek,lclblTbl)
*-- end of lfLblFound.

*!*************************************************************
*! Name      : gfDoTriger
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 07/22/99
*! Purpose   : Function to control any triggers found in the
*!             triggers file, customized processes and workflow
*!             server requests.
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : 1) lcProgName, Object ID.
*!                      2) lcEvent, Event ID.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =gfDoTriger()
*!*************************************************************
*E301297,1 this function was added by HS for the entry E301297,1.
*!*************************************************************
*
FUNCTION gfDoTriger

PARAMETERS lcProgName , lcEvent

PRIVATE lnOldAlias , lcProgToDo , laParamExp , laParam , lcParmStr ,;
        lnCount    , llReturn

llReturn = .T.

*-- If any of the parameters is not passed or passed incorrectly 
IF TYPE('lcProgName') <> 'C' .OR. EMPTY(lcProgName) .OR.;
   TYPE('lcEvent') <> 'C' .OR. EMPTY(lcEvent)
  RETURN
ENDIF

*-- Save the old alias
lnOldAlias = SELECT(0)

*-- Open the Trigger file if it was not opened
IF !USED('SYCTRIGG')
  =gfOpenFile(gcSysHome + 'SYCTRIGG' , 'OBJEVENT' , 'SH')
ENDIF

SELECT SYCTRIGG

*-- If there is triggers for this Object/Event
IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
  
  *-- Scan loop to scan the Object/Event triggers
  SCAN REST;
      WHILE cAPObjNam + cEvent_ID = PADR(lcProgName , 10) +;
            PADR(lcEvent , 10)
    
    *-- Get the name of the program that should be executed
    lcProgToDo = cTrig_ID
    
    *-- Initialize the parameter string variable
    lcParmStr  = ''
    
    *-- Restore the old alias to be able to evaluate the parameter
    *-- expressions properly
    SELECT (lnOldAlias)
    
    *-- If there is one or more parameters that should be passed to the
    *-- program
    IF !EMPTY(SYCTRIGG.mParmExpr)
      
      *-- Get the parameter expressions
      DIMENSION laParamExp[OCCURS('~' , SYCTRIGG.mParmExpr) + 1]
      =gfSubStr(SYCTRIGG.mParmExpr , @laParamExp , '~')
      
      *-- Initialize the parameters array
      DIMENSION laParam[ALEN(laParamExp , 1)]
      laParam = ""
      
      *-- Get the parameters values that will be passed to the program
      FOR lnCount = 1 TO ALEN(laParamExp , 1)
        laParam[lnCount] = EVALUATE(laParamExp[lnCount])
        lcParmStr = lcParmStr + IIF(lnCount = 1 , '' , ' , ') +;
                    'laParam[' + ALLTRIM(STR(lnCount)) + ']'
        
      ENDFOR    && End of FOR lnCount = 1 TO ALEN(laParamExp , 1)
    ENDIF    && End of IF !EMPTY(SYCTRIGG.mParmExpr)
    
    *-- If custom process
    IF SYCTRIGG.cActvTyp = 'C'
      *-- Call the program and get the returned value
      llReturn = &lcProgToDo(&lcParmStr)
    ELSE    && Else [If Server request]
    ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'
  
    SELECT SYCTRIGG
  ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...
  
  *B603662,1 BWA 05/25/2000 In case the process doesn't exist.[START]
ELSE
  llReturn = .F.
  *B603662,1 [END]
  
ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))

*-- Restore the old alias
SELECT (lnOldAlias)

RETURN (llReturn)
