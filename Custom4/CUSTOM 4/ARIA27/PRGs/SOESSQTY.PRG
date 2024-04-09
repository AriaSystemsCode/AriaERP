*:************************************************************************
*: Program file  : SOESSQTY.Prg
*: Program desc. : Screen to edit extended size scale quantity
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 04/28/99
*: REF           : *C101493,1 AMM
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOESSQTY WITH lcStyle , lcStore, lcTmpORdL , lcBrowStr , lcSeekEx , laQtyArr , laTitArr
*:************************************************************************
*E301274,1 AMM 07/06/1999 Call the extended size scale from the PO screen
*B802436,1 AMM 08/18/1999 Fix some bugs in extended size scale screen.
*B802455,1 AMM 09/08/1999 Go to next record when reaching last field in the browse
*B802585,1 AMM 09/09/1999 1- Fix the bug of wronge decimal point in numeric fields 
*B802585,1 AMM            in the extended size scale screen
*B802585,1 AMM            2- Add two objects for total quantity and amount of all sizes
*B603776,1 AMM 07/26/2000 1- Fix the wrong update of the total qty.
*E302035,1 SSH
*E302047,1 ABD 11/01/2002 Some modification in sales order - Extended Size Scale.
*C200587,1 TMI 08/07/2003 Fix the bug in standard that complete field in ordline is empty
*C200587,1 TMI            Add trigger to update ordline.complete with style.soldout
*B037225,1 NNA 01/06/2004 Fix Bug in the sales/Purchase order's style Extended S_Scale screen of wrong calculating
*B037225,1 NNA            in the Open and Booked quantity fields (Fix in the Code and the screen)
*C123158,1 NNA 07/18/2004 Call a new screen to input a Screen name,screen Color,Accessory Color for
*C123158,1 NNA            every order line that for customer FLOWERS BY ZOE
*B124731,1 NNA 10/21/2004 Fix bug that when you try to input a style-color more than time ,system
*B124731,1 NNA            open the Ext. size Scale Screen with the latest information for that style-color
*B607966,1 TMI 02/08/2007 Fix a bug that cancel button does not work
*:************************************************************************

PARAMETERS lcStyle , lcStore, lcTmpORdL , lcBrowStr , lcSeekEx , laQtyArr , laTitArr,lcCallPrg

STORE .T. TO llFromBrow
*B607966,1 TMI [Start] save the laData array to a temp array to restore if cancel button is clicked
PRIVATE laTmpArr
DIMENSION laTmpArr[ALEN(laData)]
=ACOPY(laData,laTmpArr)
*B607966,1 TMI [End  ] 

STORE SPACE(0) TO lcEscTrap , lcCtrETrap , lcCtrHTrap , lcCtrNTrap , lcOldVal,lcCtrLTrap,lcCtrRTrap
STORE SPACE(1) TO lcSize , lcGroup 
*B802585,1 AMM Initialize two variables for total quantity and amount
STORE 0 TO lnTQty,lnTAmount
*B802585,1 AMM end
lcParent = 'MULTISP'
*E302035,1 SSH
lcStyHdr   = gfItemMask("HI")

lnExtSzWd = gfGetMemVar('M_EXTWIDTH')
lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
*E302035,1 SSH

lcTitle  = 'Style/Color available sizes'
lcOrdCh1 = 'MULTISC1'
lcOrdCh2 = 'MULTISC2'
lcOrdCh3 = 'MULTISC3'
*E301274,1 AMM Add two new screens
lcOrdCh31 = 'MULTISC31'
lcOrdCh4 = 'MULTISC4'
*E301274,1 AMM end
lcTmpSize  = gfTempName()
lcTmpSty   = gfTempName()
lcTmpStyD  = gfTempName()
lcTmpScl   = gfTempName()
=gfOpenFile(gcDataDir+'STYLE','STYLE', 'SH',@lcTmpSty,.T.)
SELECT Style
SET ORDER TO Style
=gfOpenFile(gcDataDir+'STYDYE','STYDYE', 'SH',@lcTmpStyD,.T.)
=gfOpenFile(gcDataDir+'SCALE','SCALE', 'SH',@lcTmpScl,.T.)
*--- 302035
=gfOpenFile(gcDataDir+'ScaleHd','Extscale', 'SH')
*--- 302035
*E301274,1 AMM Add two arrays to control the display of objects on the screen
*DECLARE laOpenSell[8],laOldQty[9],laScl[9]
DECLARE laOpenSell[8],laOldQty[9],laScl[9],laScrTit[9], laScrfld[9]
STORE .T. TO laScrTit, laScrObj
*E301274,1 AMM end
STORE SPACE(0) TO laOpenSell
STORE 0 TO laOldQty

SELECT (lcTmpORdL)
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
FOR lnC = 1 TO ALEN(laQtyArr,1)
  lnPos = ASCAN(laFileStru,laQtyArr[lnc,1]) 
  IF lnPos # 0
    lcName = 'c'+laFileStru[lnPos]
    lnSize = laFileStru[lnPos+2]+IIF(laFileStru[lnPos+3]>0,1+laFileStru[lnPos+3],0)
    lnFileStru = lnFileStru+1
    DIMENSION laFileStru[lnFileStru,4]
    laFileStru[lnFileStru,1] = lcName
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = lnSize
    laFileStru[lnFileStru,4] = 0
  ENDIF
ENDFOR

DIMENSION laFileStru[lnFileStru+1,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = "lHedFlag"
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

DIMENSION laFileStru[lnFileStru+1,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = "cStatus"
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

*E301274,1 AMM START
DO CASE
  CASE lcCallPrg = 'PO'
    *E301274,1 AMM Add a new field for the ship to (location/customer)
    lnFileStru = lnFileStru+1
    DIMENSION laFileStru[lnFileStru,4]
    laFileStru[lnFileStru,1] = "cWarCust"
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 1
    laFileStru[lnFileStru,4] = 0
    STORE .F. TO laScrTit, laScrfld
  CASE lcCallPrg = 'SO'
    STORE .T. TO laScrTit, laScrfld
ENDCASE
*--- 302035
lnFileStru = lnFileStru+1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = "ScaleDes"
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 7
laFileStru[lnFileStru,4] = 0

*E301274,1 AMM  end

Create Table (gcWorkDir+lcTmpSize) FROM ARRAY laFileStru
SCATTER MEMVAR MEMO BLANK

lnBrR = 0
lcBrowTitl = 'Available scales'

IF lfCreSize(lcStyle)
  SELECT (lcTmpSize)
  *B802585,1 AMM Sum total quantity and amount
  DO CASE
    CASE lcCallPrg = 'SO'
      *B603776,1 AMM add a condition of not header records
      *SUM ALL &laQtyArr[9,1],&laQtyArr[9,1]*Price TO lnTQty,lnTamount
      SUM ALL &laQtyArr[9,1],&laQtyArr[9,1]*Price TO lnTQty,lnTamount FOR !lHedFlag
      *B603776,1 AMM end
  ENDCASE
  *B802585,1 AMM end
  SET RELATION TO 'S'+Scale INTO (lcTmpScl)
  SET RELATION TO STYLE INTO (lcTmpSty) ADDITIVE
  *E301274,1 AMM start set relation if called from SO.
  *SET RELATION TO Style+laData[31]+SPACE(10) INTO (lcTmpStyD) ADDITIVE
  DO CASE
    CASE lcCallPrg = 'SO'
      SET RELATION TO Style+laData[31]+SPACE(10) INTO (lcTmpStyD) ADDITIVE
    
  ENDCASE
  *E301274,1 AMM  end
  GO TOP
  SKIP 1
  laOpenSell[1] = &lcTmpStyD..Wip1+&lcTmpStyD..Stk1-&lcTmpStyD..Ord1
  laOpenSell[2] = &lcTmpStyD..Wip2+&lcTmpStyD..Stk2-&lcTmpStyD..Ord2
  laOpenSell[3] = &lcTmpStyD..Wip3+&lcTmpStyD..Stk3-&lcTmpStyD..Ord3
  laOpenSell[4] = &lcTmpStyD..Wip4+&lcTmpStyD..Stk4-&lcTmpStyD..Ord4
  laOpenSell[5] = &lcTmpStyD..Wip5+&lcTmpStyD..Stk5-&lcTmpStyD..Ord5
  laOpenSell[6] = &lcTmpStyD..Wip6+&lcTmpStyD..Stk6-&lcTmpStyD..Ord6
  laOpenSell[7] = &lcTmpStyD..Wip7+&lcTmpStyD..Stk7-&lcTmpStyD..Ord7
  laOpenSell[8] = &lcTmpStyD..Wip8+&lcTmpStyD..Stk8-&lcTmpStyD..Ord8  
  
  PUSH KEY
  ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrowTitl)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB

  *E301274,1 AMM The screen is now shared
  *DO (gcScrDir+gcWinAppl+'SOSSCL.SPX')
  *SHOW GETS
  DO (gcScrDir+'SOSSCL.SPX')
  *E301274,1 AMM end
  
  ON KEY LABEL ALT+A 
  POP KEY

  IF USED(lcTmpSize)
    USE IN (lcTmpSize)
  ENDIF
  ERASE (gcWorkDir+lcTmpSize+'.DBF')
  ERASE (gcWorkDir+lcTmpSize+'.FPT')
  IF USED(lcTmpStyD)
    =gfCloseFile(lcTmpStyD)
  ENDIF
  IF USED(lcTmpSty)
    =gfCloseFile(lcTmpSty)
  ENDIF
  IF USED(lcTmpScl)
    =gfCloseFile(lcTmpScl)
  ENDIF

ENDIF    && End of IF

*!*************************************************************
*! Name      : lfCreSize
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Purpose   : Function to colect the Style Color size scales in
*!             the temp. file lcTmpSize
*! Ref       : *C101493,1
*!*************************************************************
*! Called from : lfBrowSize
*!*************************************************************
*! Passed Parameters : Style , Color
*!*************************************************************
*! Return      : .T. if it colect any records
*!               .F. if ther is no records colected
*!*************************************************************
*
FUNCTION lfCreSize

PARAMETERS lcStyle

PRIVATE llReturn

llReturn = .F.        && Varible to hold the return value of the function

SELECT (lcTmpSty)
=SEEK(lcStyle)

SET RELATION TO lcSeekEx+STYLE INTO (lcTmpORdL) ADDITIVE
SET RELATION TO 'S'+Scale INTO (lcTmpScl) ADDITIVE

*B124731,1 NNA 10/21/2004 (Begin) Do [lfvStyCol() .AND. lfvalSty()] even it a new style-color or not
*SET SKIP TO (lcTmpORdL)
*SCAN REST ;
    WHILE Style = lcStyle ;
      FOR (!EOF(lcTmpORdL) .OR. lfvStyCol() .AND. lfvalSty())
SCAN REST ;
    WHILE Style = lcStyle FOR (lfvStyCol() .AND. lfvalSty())
*B124731,1 NNA (End)

  llReturn = .T.
  FOR lnC = 1 TO ALEN(laTitArr,1)
    m.&laTitArr[lnC,1] = laTitArr[lnC,2]
  ENDFOR
  m.lHedFlag = .T.
  FOR lnC = 1 TO 8
    lcC = STR(lnC,1)
    m.c&laQtyArr[lnC,1] = &lcTmpScl..SZ&lcC
  ENDFOR
  *B802585,1 AMM Change the title
  *m.c&laQtyArr[9,1] = 'Pieces'
  m.c&laQtyArr[9,1] = 'Tot Qty'
  *B802585,1 AMM end
  *B802585,1 AMM Use append and gather memvar instead
  *INSERT INTO (lcTmpSize) FROM MEMVAR 
  *B802585,1 AMM end
  SELECT (lcTmpSize)
  
  *B802585,1 AMM Add the title record
  APPEND BLANK
  *--- SSH 302035
  m.Scale    = &lcTmpSty..scale
  m.ScaleDes = IIF(SEEK(LEFT(ALLTRIM(m.Scale),lnExtSzWd),"ScaleHd"),ScaleHd.cDim1Desc,"")
  m.ScaleDes = IIF(SEEK("S"+m.Scale,"Scale") .AND. !EMPTY(Scale.cDim1),Scale.cDim1,m.ScaleDes)
  *--- SSH 302035
  DO CASE
    CASE lcCallPrg = 'SO'
      *C200587,1  TMI [Start] Fix the bug in standard that complete field in ordline is empty
      m.Start    = laData[9]
      m.Complete = laData[10]
      *C200587,1  TMI [End  ] 
      GATHER MEMVAR FIELDS EXCEPT Gros_Price,DISC_PCNT,Price,Comm1,Comm2
      *C200587,1  TMI [Start] Update complete field in ordline with sold out file from style file
      IF ASCAN(laEvntTrig,PADR('SOLDOUT',10)) <> 0
        =gfDoTriger(lcProgName,PADR('SOLDOUT',10))
      ENDIF      
      *C200587,1  TMI [End  ] 
    CASE lcCallPrg = 'PO'
      *--- SSHN
      *GATHER MEMVAR 
      GATHER MEMVAR FIELDS EXCEPT Gros_Price,DISC_PCNT,nCost1,Comm1,Comm2
  ENDCASE
  *B802585,1 AMM end
  
  *B124731,1 NNA 10/21/2004 (Begin) Stop this Part to Prevent System from getting the latest info.
  *B124731,1 NNA            for the Same previous style-color.  
  *IF !EOF(lcTmpORdL)
    *SELECT (lcTmpORdL)
    *SCATTER MEMVAR MEMO
    *-- Convert quantities to charachter fields.
    *FOR lnC = 1 TO ALEN(laQtyArr,1)
    *  m.c&laQtyArr[lnC,1] = STR(m.&laQtyArr[lnC,1],laQtyArr[lnC,2]+IIF(laQtyArr[lnC,3]>0,1,0),laQtyArr[lnC,3])
    *ENDFOR
    *m.lHedFlag  = .F.
    *m.cStatus   = 'S'
    *E301274,1 AMM start
    *DO CASE
    *  CASE lcCallPrg = 'PO'
    *    m.cWarCust = IIF(EMPTY(m.Account),'L','C')
    *ENDCASE
    *E301274,1 AMM  end
    *m.ScaleDes = ""
    *INSERT INTO (lcTmpSize) FROM MEMVAR  
  *ELSE
  *B124731,1 NNA (End)
  
    m.Style    = &lcTmpSty..Style
    m.Prepak   = &lcTmpSty..Prepak
    m.lHedFlag = .F.
    m.Flag     = 'N'
    m.cStatus  = 'N'
    *m.Scale    = &lcTmpSty..scale
    *m.PACK_ID  = SPACE(16) 
    *m.Group    = SPACE(1)
    *E301274,1 AMM start
    DO CASE
      CASE lcCallPrg = 'SO'
        m.PACK_ID  = SPACE(16) 
        m.Group    = SPACE(1)
        **--- SSH 302035
        *m.Scale    = &lcTmpSty..scale
        *m.ScaleDes = IIF(SEEK(LEFT(ALLTRIM(m.Scale),lnExtSzWd),"ScaleHd"),ScaleHd.cDim1Desc,"")
        *m.ScaleDes = IIF(SEEK("S"+m.Scale,"Scale") .AND. !EMPTY(Scale.cDim1),Scale.cDim1,m.ScaleDes)
        *--- SSH 302035
      CASE lcCallPrg = 'PO'
        m.cVenSty   = SPACE(19)
        m.Reference = SPACE(30)
        m.cWarCust  = SPACE(1)
        m.cWareCode = SPACE(8)
    ENDCASE
    *E301274,1 AMM end
    
    FOR lnC = 1 TO ALEN(laQtyArr,1)
      IF EMPTY(m.&laQtyArr[lnC,1])
        lcINum = REPLICATE('0',1+IIF(laQtyArr[lnC,3]=0,0,laQtyArr[lnC,3]+1)  )
        m.c&laQtyArr[lnC,1] = SPACE(laQtyArr[lnC,2]-LEN(lcINum))+lcINum
      ELSE
        m.c&laQtyArr[lnC,1] = STR(m.&laQtyArr[lnC,1],laQtyArr[lnC,2]+IIF(laQtyArr[lnC,3]>0,1,0),laQtyArr[lnC,3])      
      ENDIF
    ENDFOR
    m.ScaleDes = ""
    INSERT INTO (lcTmpSize) FROM MEMVAR  

  *B124731,1 NNA (Begin)
  *ENDIF
  *B124731,1 NNA (End)
    
  SELECT (lcTmpSty)
ENDSCAN    && End of SCAN Loop

SET RELATION TO
RETURN llReturn

*!*************************************************************
*! Name      : lfvStyCol
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Purpose   : Function Validate the style 
*! Ref       : *C101493,1
*!*************************************************************
*! Called from : lfBrowSize
*!*************************************************************
*! Passed Parameters : Style , Color
*!*************************************************************
*! Return      : .T. if VALID STYLE 
*!               .F. if Not valid style 
*!*************************************************************
*
FUNCTION lfvStyCol

lnAlias = SELECT(0)
SELECT (lcTmpSize)
SCATTER MEMVAR MEMO BLANK
IF !SEEK(lcSeekEx+&lcTmpSty..Style,lcTmpORdL )
  *-- Message : 32017
  *-- Style scale not found in the scale file. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 32018
  *-- This is a canceled style. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 32019
  *-- This style/color is on hold. 
  *-- Button : 32003
  *-- Accept Reenter

  IF (!SEEK('S'+&lcTmpSty..Scale,lcTmpScl) .AND. ;
      gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (&lcTmpSty..Status='X' .AND. ;
        gfModalGen('TRM32018B00000','ALERT')=1) ;
   .OR. (&lcTmpSty..Status='H' .AND. ;
        gfModalGen('QRM32019B3003','ALERT')=2) ;
    RETURN .F.
  ENDIF
  m.Desc1    = &lcTmpSty..Desc1
  m.Scale    = &lcTmpSty..Scale
  m.PrePak   = &lcTmpSty..PrePak
  m.Season   = &lcTmpSty..Season
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Purpose   : Function to display the Style Color Size browse
*! Ref       : *C101493,1
*!*************************************************************
*! Called from : Screen WEA200 setup
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfDispBrow

SELECT (lcTmpSize)
GO TOP

*BROWSE FIELDS &lcBrowStr;
       WINDOW (lcOrdCh2) ;
       WHEN lfwBrows() ;
       VALID lfvBrows() ;
       IN WINDOW (lcParent) ;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       NOMENU;
       SAVE;
       TITLE lcBrowTitl

IF lcCallPrg = 'SO'
  BROWSE FIELDS &lcBrowStr;
       WINDOW (lcOrdCh2) ;
       WHEN lfwBrows() ;
       VALID lfvBrows() ;
       IN WINDOW (lcParent) ;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       NOMENU;
       SAVE;
       TITLE lcBrowTitl
ELSE
  BROWSE FIELDS &lcBrowStr;
       WINDOW (lcOrdCh4) ;
       WHEN lfwBrows() ;
       VALID lfvBrows() ;
       IN WINDOW (lcParent) ;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       NOMENU;
       SAVE;
       TITLE lcBrowTitl
ENDIF
              
*-- Replace the record pointer at the first Edit record
SKIP 1
lnBrR  = RECNO()            && Varible to hold the browse record number
SHOW WINDOW (lcBrowTitl) REFRESH


*!*************************************************************
*! Name      : lfwBrows
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : WHEN Function for the Style Color Size browse
*!*************************************************************
*! Called from : Screen WEA200 setup
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwBrows

PRIVATE lcCurAlias

*-- IF the user is moving the record pointer Up and the new record
*-- is a Header record
IF lnBrR > RECNO(lcTmpSize) .AND. lHedFlag
  SKIP -1
  *-- IF the begin of file
  IF BOF(lcTmpSize)
    SKIP 1
  ENDIF    && End of IF
ENDIF    && End of IF

*--  IF the user is moving the record pointer Down and the new record
*-- is a Header record
IF lnBrR < RECNO(lcTmpSize) .AND. lHedFlag
  SKIP 1
  
 *--  IF the end of file
 IF EOF(lcTmpSize)
    SKIP -1
  ENDIF    && End of IF
ENDIF    && End of IF
SCATTER MEMVAR MEMO
*--- SSH
*E302047,1 ABD - I open this line becasue the program should get the cuurent line. [Begin]
*SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO laOldQty
SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO laOldQty
*E302047,1 ABD - [End]
*--- SSH
STORE .F. TO laScl
FOR lnC = 1 TO &lcTmpScl..Cnt
  laScl[lnC] = .T.
ENDFOR 
DO CASE
  CASE lcCallPrg = 'SO'
    laScl[9] = &lcTmpSty..Commission
    laOpenSell[1] = &lcTmpStyD..Wip1+&lcTmpStyD..Stk1-&lcTmpStyD..Ord1
    laOpenSell[2] = &lcTmpStyD..Wip2+&lcTmpStyD..Stk2-&lcTmpStyD..Ord2
    laOpenSell[3] = &lcTmpStyD..Wip3+&lcTmpStyD..Stk3-&lcTmpStyD..Ord3
    laOpenSell[4] = &lcTmpStyD..Wip4+&lcTmpStyD..Stk4-&lcTmpStyD..Ord4
    laOpenSell[5] = &lcTmpStyD..Wip5+&lcTmpStyD..Stk5-&lcTmpStyD..Ord5
    laOpenSell[6] = &lcTmpStyD..Wip6+&lcTmpStyD..Stk6-&lcTmpStyD..Ord6
    laOpenSell[7] = &lcTmpStyD..Wip7+&lcTmpStyD..Stk7-&lcTmpStyD..Ord7
    laOpenSell[8] = &lcTmpStyD..Wip8+&lcTmpStyD..Stk8-&lcTmpStyD..Ord8  
ENDCASE
lnBrR = RECNO()            && Varible to hold the browse record number
SHOW WINDOW (lcBrowTitl) REFRESH
=lfRefresh()

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Purpose   : VALID Function for the Style Color Size browse
*! Ref       : *C101493,1
*!*************************************************************
*! Called from : Screen WEA200 setup
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvBrows

PRIVATE lnChoice , lnTotQty

lnTotQty = VAL(c&laQtyArr[1,1]) + VAL(c&laQtyArr[2,1]) + VAL(c&laQtyArr[3,1]) + VAL(c&laQtyArr[4,1]) +;
                  VAL(c&laQtyArr[5,1]) + VAL(c&laQtyArr[6,1]) + VAL(c&laQtyArr[7,1]) + VAL(c&laQtyArr[8,1])

IF VAL(c&laQtyArr[9,1]) <> lnTotQty
  *-- Message : 32031
  *-- Quantity out of balance! xxx pieces ok
  *-- Button : 32000 
  *-- Yes NO
  *B802585,1 AMM Don't ask user for total quantity in each line
  *IF gfModalGen('QRM32031B32000','ALERT',STR(lnTotQty , 5))=1
    *REPLACE &laQtyArr[9,1]  WITH lnTotQty,;
            c&laQtyArr[9,1] WITH STR(lnTotQty,7)
  *ELSE    && Else
    *RETURN 0
  *ENDIF    && End of IF
  REPLACE &laQtyArr[9,1]  WITH lnTotQty,;
          c&laQtyArr[9,1] WITH STR(lnTotQty,7)
  *B802585,1 AMM end
ENDIF    && End of IF
SCATTER MEMVAR
llFrmGBrw = .T.
*E301274,1 AMM start call the function if called from SO only
DO CASE
  CASE lcCallPrg ='SO'
    *E302047,1 ABD - Call Function  lfChkTQty with flage call from template SO screen. [Begin]
    *=lfChkTQty()
    =lfChkTQty(llStores)
    *E302047,1 ABD - [End]
ENDCASE
*E301274,1 AMM end

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Function to trap the keys for the Browse
*!*************************************************************
*! Called from : Scrren WEA200 Deactivate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfBrowTrap

*-- IF The window on top is the Browse
IF WONTOP(lcBrowTitl)
  llFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL ALT+A
  lcEscTrap = ON('KEY' , 'ESC')                && Varible to hold the ESC key traping
  lcCtrETrap = ON('KEY' , 'CTRL+END')          && Varible to hold the CTRL+END key traping
  lcCtrHTrap = ON('KEY' , 'CTRL+HOME')         && Varible to hold the CTRL+HOME key traping
  lcCtrNTrap = ON('KEY' , 'CTRL+ENTER')        && Varible to hold the CTRL+ENTER key traping
  ON KEY LABEL ESC DO lfTraps WITH 3
  ON KEY LABEL CTRL+END DO lfTraps WITH 4
  ON KEY LABEL CTRL+HOME DO lfTraps WITH 5
  ON KEY LABEL CTRL+ENTER DO lfTraps WITH 6
ENDIF    && End of IF

RETURN .F.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*! Called from : Scrren WEA200 Activate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcBrowTitl) .AND. llFromBrow
  llFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrowTitl)
  ON KEY LABEL ESC &lcEscTrap
  ON KEY LABEL CTRL+END &lcCtrETrap
  ON KEY LABEL CTRL+HOME &lcCtrHTrap
  ON KEY LABEL CTRL+ENTER &lcCtrNTrap
ENDIF
  
*!*************************************************************
*! Name      : lfTraps
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Function to be called from some traped keys
*!*************************************************************
*! Called from : lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 , 2
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfTraps

PARAMETERS lnTrap

DO CASE 
  *-- Case TAB
  CASE lnTrap = 1
    ACTI WINDOW (lcOrdCh3) TOP
    
    *-- IF the system Edit order lines notes is Yes
    *E301274,1 AMM adjust condition
    *IF laSetups[3,2]='Y'
    IF lcCallPrg = 'SO' .AND. laSetups[3,2]='Y' 
      _CUROBJ = OBJNUM(pbNotes)
    ELSE    && Else
      _CUROBJ = OBJNUM(pbOk)
    ENDIF    && End of IF
    *E301274,1 AMM end
  *-- Case BACKTAB
  CASE lnTrap = 2
    
    *ACTI WINDOW (lcOrdCh1) TOP  
    *_CUROBJ = OBJNUM(lcGroup)
    IF lcCallPrg = 'SO'
      ACTI WINDOW (lcOrdCh1) TOP  
      _CUROBJ = OBJNUM(lcGroup)
    ELSE
      ACTI WINDOW (lcOrdCh3) TOP
      _CUROBJ = OBJNUM(pbCansel)
    ENDIF
    
  *-- Case ESC
  CASE lnTrap = 3
    RETURN
    
  *-- Case CTRL+END
  CASE lnTrap = 4
    GO BOTTOM
  
  *-- Case CTRL+HOME
  CASE lnTrap = 5
    GO TOP
    SKIP 1
  
  *-- Case CTRL+ENTER
  CASE lnTrap = 6
    RETURN
  
ENDCASE

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Called from : Screen WEA200 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
FUNCTION lfwIbBrow

*-- IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+A}" CLEAR 
  RETURN .T.
ENDIF    && End of IF

RETURN .F.

*!*************************************************************
*! Name      : lfwOV
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : When function to get the old value
*!*************************************************************
*! Called from : Screen WEA200
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfwOV
IF lHedFlag
  RETURN .F.
ELSE
  lcOldVal = EVALUATE(SYS(18))    && Varible to hold the old value of the curent field
ENDIF

DO CASE
  CASE UPPER(VARREAD()) = 'CPPQTY'
    RETURN !EMPTY(Prepak)
  *B802585,1 AMM Treat with the numeric field not the string one
  *CASE UPPER(VARREAD()) = 'CGROS_PRICE' .AND. lcCallPrg = 'SO'
  CASE lcCallPrg = 'SO' .AND. UPPER(VARREAD()) = 'GROS_PRICE' 
  *B802585,1 AMM end
    RETURN lfwGPrice()
  *E301274,1 AMM 
  CASE lcCallPrg = 'PO' .AND. INLIST(UPPER(VARREAD()) , 'CGROS_PRICE' ,'CDISC_PCNT' , 'CNCOST1' )
    RETURN !(lFlag) AND !(llDyelot AND llDyltYes AND EMPTY(m.Dyelot)) AND ;
      !(lcAType='R' AND !EMPTY(laData[71]))

  *E301274,1 AMM Adjust condition
  *CASE INLIST(UPPER(VARREAD()),'CDISC_PCNT','CPRICE')
  *B802585,1 AMM Treat with the numeric field not the string one
  *CASE lcCallPrg = 'SO' .AND. INLIST(UPPER(VARREAD()),'CDISC_PCNT','CPRICE')
  CASE lcCallPrg = 'SO' .AND. INLIST(UPPER(VARREAD()),'DISC_PCNT','PRICE')
  *B802585,1 AMM end
  *E301274,1 AMM end
    RETURN !m.lContract
  *E301274,1 AMM Start
  *CASE INLIST(UPPER(VARREAD()),'CCOMM1','CCOMM2')
  *B802585,1 AMM Treat with the numeric field not the character one
  *CASE lcCallPrg = 'SO' .AND. INLIST(UPPER(VARREAD()),'CCOMM1','CCOMM2')
  CASE lcCallPrg = 'SO' .AND. INLIST(UPPER(VARREAD()),'COMM1','COMM2')
  *B802585,1 AMM end
  *E301274,1 AMM end
    RETURN laScl[9]
  *E301274,1 AMM
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='STORE'
    RETURN cWarCust = 'C' AND !EMPTY(cWareCode) AND lfOldValue()
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='CWARECODE'
    RETURN lfOldValue()
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='DYELOT'
   RETURN lfOldValue()
  
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='CNCOST2'
    RETURN llEdtLnCst AND !lFlag AND !(llDyelot AND llDyltYes AND EMPTY(m.Dyelot)) AND !(lcAType='R' AND !EMPTY(laData[71])) AND !(lcIType2 $ 'FS')
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='CNCOST3'
    RETURN llEdtLnCst AND !lFlag AND !(llDyelot AND llDyltYes AND EMPTY(m.Dyelot)) AND !(lcAType='R' AND !EMPTY(laData[71])) AND !(lcIType3 $ 'FS')
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='CNCOST4'
    RETURN llEdtLnCst AND !lFlag AND !(llDyelot AND llDyltYes AND EMPTY(m.Dyelot)) AND !(lcAType='R' AND !EMPTY(laData[71])) AND !(lcIType4 $ 'FS')
  CASE lcCallPrg = 'PO' .AND. UPPER(VARREAD())='CNCOST5'
    RETURN llEdtLnCst AND !lFlag AND !(llDyelot AND llDyltYes AND EMPTY(m.Dyelot)) AND !(lcAType='R' AND !EMPTY(laData[71])) AND !(lcIType5 $ 'FS')
ENDCASE


*!*************************************************************
*! Name      : lfvOk
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Valid function of the push button pbOk
*!*************************************************************
*! Called from : Screen WEA200
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvOk
SELECT (lcTmpSize)
*B802585,1 AMM Calculate total quantity and cpompare it with the entered one

DO CASE
  CASE lcCallPrg = 'SO'
    *B603776,1 AMM exclude header records from the sum condition
    *SUM ALL &laQtyArr[9,1],&laQtyArr[9,1]*Price TO lnNTQty,lnNAmount
    SUM ALL &laQtyArr[9,1],&laQtyArr[9,1]*Price TO lnNTQty,lnNAmount FOR !lHedFlag
    *B603776,1 AMM end
    *E302047,1 ABD - Sum the array at the current array. [Begin]
  CASE lcCallPrg = 'PO'
    SUM ALL &laQtyArr[9,1],&laQtyArr[9,1]*Price TO lnNTQty,lnNAmount FOR !lHedFlag    
    *E302047,1 ABD - [End]
ENDCASE
*--- SSH
*IF lnNTQty # lnTQty
IF lnNTQty # (lnTQty)
*--- SSH
  *-- Message : 32031
  *-- Quantity out of balance! xxx pieces ok
  *-- Button : 32000 
  *-- Yes NO
  IF gfModalGen('QRM32031B32000','ALERT',STR(lnNTQty , 5))#1
    RETURN
  ENDIF
ENDIF
*B802585,1 AMM end
SCAN FOR !lHedFlag .AND. TOTQTY # 0 
  DO CASE
    CASE cStatus = 'N'
      SCATTER MEMVAR MEMO
      DO CASE
        CASE lcCallPrg = 'SO'
          *B802436,1 AMM Comment out
          *IF !EMPTY(lcGroup)
            *M.Group = lcGroup
          *ENDIF
          *B802436,1 AMM end
          M.Store = lcStore
          *-- Increment line no
          lnLines     = lnLines + 1
          m.LineNo    = lnLines
        CASE lcCallPrg = 'PO'
          *-- Increment line no
          lnLines     = lnLines + 1
          m.LineNo    = lnLines
          m.cWarecode = ladata[19]
      ENDCASE
      INSERT INTO (lcTmpORdL) FROM MEMVAR
    CASE cStatus = 'M'
      SCATTER MEMVAR MEMO
      DO CASE
        CASE lcCallPrg = 'SO'
          *B802436,1 AMM Comment out
          *M.Group = lcGroup
          *B802436,1 AMM end
          SELECT (lcTmpORdL)
          IF SEEK(lcSeekEx+&lcTmpSize..Style+STR(&lcTmpSize..LineNo,6) )
            GATHER MEMVAR MEMO
          ENDIF
        CASE lcCallPrg = 'PO'
          SELECT (lcTmpORdL)
          IF SEEK(&lcTmpSize..Style+STR(&lcTmpSize..LineNo,6) )
            m.cWarecode = ladata[19]
            GATHER MEMVAR MEMO
          ENDIF
      ENDCASE
  ENDCASE
ENDSCAN

*C123158,1 NNA 07/18/2004 (BEGIN) Display the Operation manager Screen for FLO09 after the user
*C123158,1 NNA             pressed 'OK' and that if Totqty <>0 and the ESS. not from Template Button
IF ASCAN(laEvntTrig,PADR('DISPSCR',10))<>0 .AND. EVAL(LCTMPORDL+'.TOTQTY')>0 ;
  .AND. !llTempFlag
  =gfDoTriger(lcProgName,PADR('DISPSCR',10))
ENDIF
*C123158,1 NNA (End)

SET ORDER TO Styqlty IN STYLE
CLEAR READ

*!*************************************************************
*! Name      : lfCtoN
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Function to fix the chrecter fields picture
*!*************************************************************
*! Called from : The Style Color size scales browse
*!*************************************************************
*! Passed Parameters : The field width , The number of decimal places
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCtoN

PARAMETERS lnFieldLen , lnDecmlLen

lnFieldLen = IIF(TYPE('lnFieldLen') <> 'N' , 0 , lnFieldLen)
lnDecmlLen = IIF(TYPE('lnDecmlLen') <> 'N' , 0 , lnDecmlLen)

PRIVATE lcFieldNam,lcnField
lcFieldNam = UPPER(VARREAD())        && Varible to hold the curent field name

*-- IF the field width parameter was not send or was send with 0
IF lnFieldLen = 0
  REPLACE &lcFieldNam WITH STRTRAN(&lcFieldNam , ' ' , '')
ELSE 
  REPLACE &lcFieldNam WITH STR(VAL(STRTRAN(&lcFieldNam , ' ' , '')) , lnFieldLen , lnDecmlLen)
ENDIF
lnPos = ASCAN(laQtyArr,SUBSTR(lcFieldNam,2))
IF lnPos # 0
  lcnField = laQtyArr[lnPos]
  IF &lcnField # VAL(EVAL(lcFieldNam))
    *E301274,1 AMM start
    *REPLACE &lcnField WITH VAL(EVAL(lcFieldNam)),;
            cStatus   WITH IIF(cStatus='N','N','M'),;
            FLAG      WITH IIF(FLAG='N',FLAG,'M')
    REPLACE &lcnField WITH VAL(EVAL(lcFieldNam)),;
            cStatus   WITH IIF(cStatus='N','N','M')
    *E301274,1 AMM end
    DO CASE
      CASE lcCallPrg = 'SO'
        REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
    ENDCASE
  ENDIF

ENDIF
SCATTER MEMVAR MEMO

*!*************************************************************
*! Name      : lfvF
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Function to validate browse fields
*!*************************************************************
*! Called from : The Style Color size scales browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvF

DO CASE
  CASE lcCallPrg='PO'
    DO CASE
      CASE UPPER(VARREAD()) = 'CPPQTY'
        =lfvPPakQty(.T.)
      CASE UPPER(VARREAD()) = 'PREPAK'
        =lfvPrePack(.T.)
      CASE UPPER(VARREAD()) = 'CTOTQTY'
        =lfCtoN()
        =lfvPieces(.T.)
      CASE UPPER(VARREAD()) = 'CQTY1'
        =lfCtoN()
        =lfexSzQty(1)
      CASE UPPER(VARREAD()) = 'CQTY2'
        =lfCtoN()
        =lfexSzQty(2)
      CASE UPPER(VARREAD()) = 'CQTY3'
        =lfCtoN()
        =lfexSzQty(3)
      CASE UPPER(VARREAD()) = 'CQTY4'
        =lfCtoN()
        =lfexSzQty(4)
      CASE UPPER(VARREAD()) = 'CQTY5'
        =lfCtoN()
        =lfexSzQty(5)
      CASE UPPER(VARREAD()) = 'CQTY6'
        =lfCtoN()
        =lfexSzQty(6)
      CASE UPPER(VARREAD()) = 'CQTY7'
        =lfCtoN()
        =lfexSzQty(7)
      CASE UPPER(VARREAD()) = 'CQTY8'
        =lfCtoN()
        =lfexSzQty(8)
      CASE UPPER(VARREAD()) = 'CGROS_PRIC'
        =lfCtoN(8,2)
        =lfvGPrice(.T.)
      CASE UPPER(VARREAD()) = 'CDISC_PCNT'
        =lfCtoN(4,2)
        =lfvPrcDisc(.T.)
      CASE UPPER(VARREAD()) = 'CNCOST1'
        =lfCtoN(9,2)
        =lfVCost('1',.F.,.T.)
      CASE UPPER(VARREAD()) = 'CWARCUST'
        =lfvPOPUP()
      CASE UPPER(VARREAD()) = 'CWARECODE'
        =lfvLShpTo(.T.)
      CASE UPPER(VARREAD()) = 'STORE'
        =lfvLStore(.T.)
      CASE UPPER(VARREAD()) = 'CNCOST2'
        =lfCtoN(9,2)
        =lfVCost('2',.F.,.T.)
      CASE UPPER(VARREAD()) = 'CNCOST3'
        =lfCtoN(9,2)
        =lfVCost('3',.F.,.T.)
      CASE UPPER(VARREAD()) = 'CNCOST4'
        =lfCtoN(9,2)
        =lfVCost('4',.F.,.T.)
      CASE UPPER(VARREAD()) = 'CNCOST5'
        =lfCtoN(9,2)
        =lfVCost('5',.F.,.T.)
    ENDCASE
  *B802436,1 AMM (START)
  CASE lcCallPrg = 'SO'
    DO CASE
      *B802436,1 AMM If the user changed Group, mark that.
      CASE UPPER(VARREAD()) = 'GROUP'
        REPLACE cStatus  WITH IIF(cStatus='N',cStatus,'M'),;
                Flag     WITH IIF(Flag='N',Flag,'M')
        *B802455,1 AMM Go to first field in the next record
        KEYBOARD '{DNARROW}'
        *B802455,1 AMM end
      *B802585,1 AMM Call valid functions of fields
      CASE UPPER(VARREAD()) = 'GROS_PRICE'
        SCATTER MEMVAR MEMO
        REPLACE cStatus  WITH IIF(cStatus='N',cStatus,'M'),;
                Flag     WITH IIF(Flag='N',Flag,'M')
        =lfvGPrice(.F.,.T.)

      CASE UPPER(VARREAD()) = 'DISC_PCNT'
        SCATTER MEMVAR MEMO
        REPLACE cStatus  WITH IIF(cStatus='N',cStatus,'M'),;
                Flag     WITH IIF(Flag='N',Flag,'M')
        =lfvPrcDisc(.F.,.T.)

      CASE UPPER(VARREAD()) = 'PRICE'
        SCATTER MEMVAR MEMO
        REPLACE cStatus  WITH IIF(cStatus='N',cStatus,'M'),;
                Flag     WITH IIF(Flag='N',Flag,'M')
        =lfvNPrice(.F.,.F.,.T.)
      CASE UPPER(VARREAD()) = 'COMM1'
        SCATTER MEMVAR MEMO
        REPLACE cStatus  WITH IIF(cStatus='N',cStatus,'M'),;
                Flag     WITH IIF(Flag='N',Flag,'M')
        =lfvComm('1',.F.,.T.)
      CASE UPPER(VARREAD()) = 'COMM2'
        SCATTER MEMVAR MEMO
        REPLACE cStatus  WITH IIF(cStatus='N',cStatus,'M'),;
                Flag     WITH IIF(Flag='N',Flag,'M')
        =lfvComm('2',.F.,.T.)
      *B802585,1 AMM end
    ENDCASE
  *B802436,1 AMM end
ENDCASE

*!*************************************************************
*! Name      : lfvLineNote
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 04/28/1999
*! Ref       : *C101493,1
*! Purpose   : Order line notepad
*!*************************************************************
*! Calls     : ARLNOTES.SPX
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvLineNote()
*!*************************************************************
FUNCTION lfvNote

DO (gcScrDir+"ARLNOTES.SPX")
REPLACE cStatus WITH IIF(cStatus='N',cStatus,'M'),;
        Flag    WITH IIF(Flag='N',Flag,'M')

*!*************************************************************
*! Name      : lfvGroup 
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 08/18/1999
*! Ref       : *B802436,1 AMM
*! Purpose   : valid function of style group
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvGroup()
*!*************************************************************
FUNCTION lfvGroup 

lnGalias = SELECT(0)
IF lcGroup # lcOldVal
  SELECT (lcTmpSize)
  SCAN FOR !lHedFlag .AND. Group # lcGroup
    REPLACE GROUP     WITH lcGroup,;
            cStatus   WITH IIF(cStatus='N',cStatus,'M'),;
            Flag      WITH IIF(Flag='N',Flag,'M')

  ENDSCAN
  GO TOP
ENDIF
SELECT (lnGalias)


*--- SSH E302035
*!*************************************************************
*! Name      : FUNCTION lfvPrePack
*! Developer : Ahmed Salah Shalaby
*! Date      : 07/01/2002
*! Purpose   : Validate Style prepak
*!*************************************************************
*! Calls     : gfPrePBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPrePack()
*!*************************************************************
FUNCTION lfvPrePack
PARAMETERS llStores
PRIVATE lcBrFields,lcFile_Ttl
*E302047,1 ABD - do not Browse in case press by the mouse out of the field. [Begin]
IF MDOWN()
  RETURN
ENDIF
*E302047,1 ABD - [End]

IF llBrowse .OR. (!EMPTY(m.Prepak) .AND. !lfChkPk(m.Scale,m.Prepak))
  *E302047,1 ABD - Call local funtion for the PrePack. [Begin]
  *=gfPrePBrow(LEFT(ALLTRIM(m.Scale),lnExtSzWd),@m.Prepak))
  = lfPrePBrow(LEFT(ALLTRIM(m.Scale),lnExtSzWd),@m.Prepak))
  *E302047,1 ABD - [End]
ENDIF
llBrowse = .F.
=SEEK('S'+m.Scale,'Scale')
IF Prepak <> lcOldVal
  llCUpdate = .T.
  =RLOCK()
  REPLACE Prepak WITH m.Prepak,;
         cStatus WITH IIF(cStatus='N',cStatus,'M')
  UNLOCK
  
  *E302047,1 ABD - Update the flage With in in case 'SO' .[Begin]
  IF lcCallPrg = 'SO'
    =RLOCK()
    REPLACE Flag WITH IIF(Flag='N','N','M')
    UNLOCK
  ENDIF
  *E302047,1 ABD - [End]
  
ENDIF
IF EMPTY(m.Prepak)
  =RLOCK()
  REPLACE PPQty   WITH 0 ,;
          cPPqty  WITH '0',;
          cStatus WITH IIF(cStatus='N',cStatus,'M')
  UNLOCK
  m.PPQTY = 0
  SHOW GET m.PPQty DISABLE
ELSE
  REPLACE Prepak  WITH m.Prepak
  SHOW GET m.PPQty ENABLE
ENDIF

*!*************************************************************
*! Name      : FUNCTION lfvPPakQty
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style prepak quantity
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPPakQty()
*!*************************************************************
FUNCTION lfvPPakQty
PARAMETERS llStores
IF m.PPQTY <> lcOldVal
  =lfvPPiec(.F.,.T.)
ENDIF
RETURN
IF m.PPQty <> 0
  =SEEK('P'+m.Scale+m.Prepak,'Scale')
  IF !llStores .AND. Picked .AND. ;
     (m.PPQty*Scale.PP1 < Pik1 .OR. ;
      m.PPQty*Scale.PP2 < Pik2 .OR. ;
      m.PPQty*Scale.PP3 < Pik3 .OR. ;
      m.PPQty*Scale.PP4 < Pik4 .OR. ;
      m.PPQty*Scale.PP5 < Pik5 .OR. ;
      m.PPQty*Scale.PP6 < Pik6 .OR. ;
      m.PPQty*Scale.PP7 < Pik7 .OR. ;
      m.PPQty*Scale.PP8 < Pik8)
    *E300420,1 Message : 32015
    *E300420,1 Quantity cannot be below than what has been picked for this style/color
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32015B00000','ALERT')
    STORE PPQty TO m.PPQty
    =SEEK('S'+m.Scale,'Scale')
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  lnPPQ1 = Scale.PP1
  lnPPQ2 = Scale.PP2
  lnPPQ3 = Scale.PP3
  lnPPQ4 = Scale.PP4
  lnPPQ5 = Scale.PP5
  lnPPQ6 = Scale.PP6
  lnPPQ7 = Scale.PP7
  lnPPQ8 = Scale.PP8
  =SEEK('S'+m.Scale,'Scale')
  FOR lnI = 1 TO 8
    lcCnt = STR(lnI,1)
    m.Qty&lcCnt = m.PPQty * lnPPQ&lcCnt
    =lfvSizeQty(lcCnt,llStores)
  ENDFOR
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  SHOW GET m.TotQty DISABLE
  =SEEK('S'+m.Scale,'Scale')
ELSE
  IF !Picked
    STORE 0 TO m.TotQty
    SHOW GET m.TotQty ENABLE
  ENDIF
ENDIF
SELECT (lcTmpSize)
=RLOCK()
REPLACE PPQty WITH m.PPQty ,;
        FLAG  WITH IIF(llStores,Flag,IIF(Flag='N','N','M'))
UNLOCK  
FOR lnC = 1 TO ALEN(laQtyArr,1)
  m.c&laQtyArr[lnC,1] = STR(m.&laQtyArr[lnC,1],laQtyArr[lnC,2]+IIF(laQtyArr[lnC,3]>0,1,0),laQtyArr[lnC,3])
ENDFOR
m.cStatus = IIF(&lcTmpSize..cStatus='N','N','M')
SELECT (lcTmpSize)
GATHER MEMVAR
*--- SSH E302035



*!*************************************************************
*! Name      : FUNCTION lfvPPiec
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : Validate Style pieces
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvPPiec()
*!*************************************************************
FUNCTION lfvPPiec
PARAMETERS llStores , llFromPk

IF !llFromPk .AND. lnTQty = lcOldVal
  RETURN
ENDIF
IF lnTQty < 0 
  *B603449,1 Message : 42000
  *B603449,1 Negative values are not allowed.
  *B603449,1 Button  : 40011
  *B603449,1 Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  m.TotQty = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF EMPTY(m.Prepak)
  RETURN
ELSE
  SELECT (lcTmpSize)
  SET FILTER TO !lHedFlag
  COUNT FOR !lHedFlag TO lnTotLin 
  SELECT Scale
  SUM ppTot TO lnTot2Cnt FOR Type+scale+prepak = 'P'+LEFT(ALLTRIM(m.Scale),lnExtSzWd);
                             .AND.   prepak = m.Prepak
  IF llFromPk
    lnTQty = lnTot2Cnt*m.PPQty
  ELSE
    m.PPQty = lnTQty/lnTot2Cnt
  ENDIF
  SHOW GET lnTQty
  SHOW GET m.PPQty
  SELECT (lcTmpSize)
  GO TOP
  m.TotQty = lnTQty
ENDIF
IF SEEK('P'+m.Scale+m.Prepak,'Scale')
  *E300408,1 Message : 32030
  *E300408,1 Total quantity is not evenly divisible by the prepak quantity xxx
  *E300408,1 Button : 00000 
  *E300408,1 Ok
  IF MOD(m.TotQty,lnTot2Cnt) <> 0 .AND. ;
    gfModalGen('TRM32030B00000','ALERT',STR(lnTot2Cnt,4))=1
    *E302047,1 ABD - [Begin]
    *REPLACE cTotQty WITH lcOldVal,;
    *        TotQty  WITH VAL(lcOldVal)
    *REPLACE cTotQty WITH STR(lcOldVal,7),;
    *        TotQty  WITH lcOldVal
    lnTQty = lcOldVal
    SET FILTER TO
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    SET FILTER TO
    RETURN
  ELSE
      REPLACE cTotQty WITH STR(lcOldVal,7),;
            TotQty  WITH lcOldVal
    *E302047,1 ABD - [End]
  ENDIF
  SELECT (lcTmpSize)
  lnTAmount =0
  SCAN
    =SEEK("P"+scale+m.Prepak,"Scale")
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO laOldQty
    REPLACE Qty1   WITH  (lnTQty*Scale.PP1/lnTot2Cnt),;
            Qty2   WITH  (lnTQty*Scale.PP2/lnTot2Cnt),;
            Qty3   WITH  (lnTQty*Scale.PP3/lnTot2Cnt),;
            Qty4   WITH  (lnTQty*Scale.PP4/lnTot2Cnt),;
            Qty5   WITH  (lnTQty*Scale.PP5/lnTot2Cnt),;
            Qty6   WITH  (lnTQty*Scale.PP6/lnTot2Cnt),;
            Qty7   WITH  (lnTQty*Scale.PP7/lnTot2Cnt),;
            Qty8   WITH  (lnTQty*Scale.PP8/lnTot2Cnt),;
            TotQty WITH  Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

    REPLACE cQty1   WITH STR((lnTQty*Scale.PP1/lnTot2Cnt),6),;
            cQty2   WITH STR((lnTQty*Scale.PP2/lnTot2Cnt),6),;
            cQty3   WITH STR((lnTQty*Scale.PP3/lnTot2Cnt),6),;
            cQty4   WITH STR((lnTQty*Scale.PP4/lnTot2Cnt),6),;
            cQty5   WITH STR((lnTQty*Scale.PP5/lnTot2Cnt),6),;
            cQty6   WITH STR((lnTQty*Scale.PP6/lnTot2Cnt),6),;
            cQty7   WITH STR((lnTQty*Scale.PP7/lnTot2Cnt),6),;
            cQty8   WITH STR((lnTQty*Scale.PP8/lnTot2Cnt),6),;
            CTotQty WITH STR(TotQty,7),;
            cStatus WITH IIF(cStatus='N','N','M'),;
            Prepak  WITH m.Prepak,;
            ppQty   WITH m.PPQty,;
            Scale   WITH Scale.Scale
    lnTAmount = lnTAmount +(TotQty*Price)
    SHOW GET lnTAmount
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    IF lcCallPrg = 'PO'
      FOR lnSzInd = 1 TO 8
        =lfexSzQty(lnSzInd)
      ENDFOR
    ELSE
      *B037225,1 NNA 01/06/2004 (Begin) go to function (lfChlTqty) if the user didn't use the Templates
	  *B037225,1 NNA 			and also if the user pushed the (New) Button Even before or after using the temp.
	  IF !llTempFlag
	    =lfChkTQty()
	  ENDIF
      *B037225,1 NNA (End)
    ENDIF
  ENDSCAN
  GO TOP
  SET FILTER TO
  IF lcCallPrg = 'SO' .AND. (Picked .AND.  ;
     (VAL(m.cQty1) < Pik1 .OR. VAL(m.cQty2) < Pik2 .OR. VAL(m.cQty3) < Pik3 .OR. VAL(m.cQty4) < Pik4 .OR. ;
      VAL(m.cQty5) < Pik5 .OR. VAL(m.cQty6) < Pik6 .OR. VAL(m.cQty7) < Pik7 .OR. VAL(m.cQty8) < Pik8))
    *E300420,1 Message : 32015
    *E300420,1 Quantity cannot be below than what has been picked for this style/color
    *E300420,1 Button : 00000
    *E300420,1 Ok
    =gfModalGen('TRM32015B00000','ALERT')
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    _CUROBJ = _CUROBJ
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
ENDIF

=SEEK('S'+m.Scale,'Scale')
IF VAL(m.cQty1)=Qty1 AND VAL(m.cQty2)=Qty2 AND VAL(m.cQty3)=Qty3 AND VAL(m.cQty4)=Qty4 AND ;
   VAL(m.cQty5)=Qty5 AND VAL(m.cQty6)=Qty6 AND VAL(m.cQty7)=Qty7 AND VAL(m.cQty8)=Qty8 AND m.TotQty=TotQty
  RETURN
ENDIF

SELECT (lcTmpSize)
GO TOP


FUNCTION lfChkPk
PARAMETER lcPScale,lcpPrepak
PRIVATE ll2Return  , lcOldAls
lcOldAls = SELECT(0)
ll2Return = .F.

IF SEEK('P'+LEFT(ALLTRIM(lcPScale),lnExtSzWd),'Scale')
 *--- +m.Prepak
 SELECT Scale
 LOCATE REST WHILE type+scale+prepak = 'P'+LEFT(ALLTRIM(lcPScale),lnExtSzWd);
             FOR   prepak = lcpPrepak
 ll2Return = FOUND()     
ENDIF

SELECT(lcOldAls)
RETURN (ll2Return)
*:*************************************************************
*: Name      : lfPrePBrow
*: Developer : Abdou Elgendy [Abd]
*: Date      : 07/01/1996
*: Purpose   : Browse the Prepack with a Bussines wise.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  =lfPrePBrow()
*:*************************************************************
*:
FUNCTION lfPrePBrow
PARAMETER lcScale,lcPrePack
PRIVATE lnOldAlias

IF llBrowse OR (!EMPTY(lcPrePack) AND !SEEK('P'+lcScale+lcPrePack,'SCALE'))

  IF SEEK('P'+lcScale,'SCALE')
    lnOldAlias = SELECT()
    *- Create a temp Files for scale File.
    lcTmpScale = gfTempName()
    SELECT SCALE
    =AFIELDS(laFileStru)
    Create Table (gcWorkDir+lcTmpScale) FROM ARRAY laFileStru
    INDEX ON PrePak TAG (lcTmpScale)
    SELECT (lcTmpScale)

    SELECT SCALE  
    SCAN REST WHILE type+scale+prepak = 'P'+lcScale
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpScale) FROM MEMVAR
    ENDSCAN
    SELECT (lcTmpScale)
    LOCATE    
    lnTotQty = 0
    lcPrePak = PrePak
    SCAN
      IF lcPrePak # PrePak
        lnOldRec = RECNO()
        =SEEK(lcPrePak)
        REPLACE PpTOt WITH lnTotQty
        lnTotQty = 0
        GOTO lnOldRec
        lcPrePak = PrePak
      ENDIF
      lnTotQty = lnTotQty + PpTot
      REPLACE PpTOt WITH 0
    ENDSCAN
    =SEEK(lcPrePak)
    REPLACE PpTOt WITH lnTotQty
    
    SELECT (lcTmpScale)
    SET RELATION TO 'S'+Scale INTO SCALE

  

    
    SELECT SCALE
    DECLARE laValues[1]  && array to get values from browse
    lcFile_Ttl = 'Prepacks'
    lcBrFields = "lcPePack = IIF(PpTot = 0,'**************',PADL(Prepak,5)+'-'+PADL(STR(PpTot,6),6)):H='Prepack',"+;
                 "lcSz1 = IIF(EMPTY(Scale.Sz1),'',PADL(Scale.Sz1,5)+'-'+PADL(Str(Pp1,3),3)):H='Size1',"+;
                 "lcSz2 = IIF(EMPTY(Scale.Sz2),'',PADL(Scale.Sz2,5)+'-'+PADL(Str(Pp2,3),3)):H='Size2',"+;
                 "lcSz3 = IIF(EMPTY(Scale.Sz3),'',PADL(Scale.Sz3,5)+'-'+PADL(Str(Pp3,3),3)):H='Size3',"+;
                 "lcSz4 = IIF(EMPTY(Scale.Sz4),'',PADL(Scale.Sz4,5)+'-'+PADL(Str(Pp4,3),3)):H='Size4',"+;
                 "lcSz5 = IIF(EMPTY(Scale.Sz5),'',PADL(Scale.Sz5,5)+'-'+PADL(Str(Pp5,3),3)):H='Size5',"+;
                 "lcSz6 = IIF(EMPTY(Scale.Sz6),'',PADL(Scale.Sz6,5)+'-'+PADL(Str(Pp6,3),3)):H='Size6',"+;
                 "lcSz7 = IIF(EMPTY(Scale.Sz7),'',PADL(Scale.Sz7,5)+'-'+PADL(Str(Pp7,3),3)):H='Size7',"+;
                 "lcSz8 = IIF(EMPTY(Scale.Sz8),'',PADL(Scale.Sz8,5)+'-'+PADL(Str(Pp8,3),3)):H='Size8'"
                 
                 
 
    SELECT (lcTmpScale)
    LOCATE
     lcBrflFont = "Courier New"
     =ARIABROW('',lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','PREPAK','laValues')
     lcPrePack = IIF(EMPTY(laValues[1]),SPACE(1),laValues[1])
    SELECT (lcTmpScale)
    SET RELATION TO 
    SELECT (lnOldAlias)
  ELSE
    =gfDialog("I",'There is no prepak for scale '+ lcScale)
    lcPrePack = SPACE(1)
  ENDIF  
ENDIF  

RETURN !EMPTY(lcPrePack)
*-- End Of lfPrePBrow

*:**************************************************************************
*:* Name        : lfvCancel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/08/2007
*:* Purpose     : restore the laData array from the temp array laTmpArr
*:***************************************************************************
*B607966,1 TMI
FUNCTION lfvCancel
=ACOPY(laTmpArr,laData)
*-- end of lfvCancel.