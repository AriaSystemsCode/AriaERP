*!********************************************************************
*: Program file  : SoICB10 (Refere to C102093)
*: Program desc. : Create order from text file.
*: For screen    : None.
*:         System: ARIA27 
*:         Module: SO
*:           Date: 12/11/2000
*:      Developer: Ahmed Mohamed EL_Anwar (AME)
*!********************************************************************
* NOTE           : This Program is based on ICB200
*!********************************************************************

lcfile= GETFILE('TXT','Choose the text file')
IF EMPTY(lcFile)
  *-- Message : < Operation has been cancelled >
  *-- Buttons : <             OK               >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
  RETURN
ENDIF
*-- Checking if User selected File not of Type Text Extension.
IF !(UPPER(RIGHT(lcFile,3))=="TXT")
  *-- Message : < Only Text files are allowed for conversion >
  *-- Buttons : <                      OK                    >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Only Text files are allowed for conversion.')  
  RETURN
ENDIF

CREATE CURSOR lcTemp (MemoF M)
APPEND BLANK
APPEND MEMO MemoF from  &lcfile

IF LEN(MemoF) =0 
  *-- Message : < The File you specified is empty; the process cannot be completed>
  *-- Buttons : <                               OK                             >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"The File you specified is empty; the process cannot be completed.")  
  RETURN
ENDIF

lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 85
=gfOpenFile(GcDataDir+'Sequence','','SH')
=gfOpenFile(GcDataDir+'Style',GcDataDir+'Style','SH')
=gfOpenFile(GcDataDir+'Customer',GcDataDir+'Customer','SH')
=gfOpenFile(GcDataDir+'OrdHdr',GcDataDir+'OrdHdr','SH')
=gfOpenFile(GcDataDir+'OrdLine',GcDataDir+'OrdLinSt','SH')
=gfOpenFile(GcDataDir+'WareHous',GcDataDir+'WareHous','SH')

GO TOP
lcWareCode = cWareCode
=gfOpenFile(GcDataDir+'Spck_Lin',GcDataDir+'Spck_Lin','SH')
lnNotLine = 1
lnMemLins = MEMLINES(lcTemp.MemoF)
*llExcept  = .F.     && Variable to check if any exception happened.
llFirst   = .T.
llMulti   = .F.
lcCust    = SUBSTR(MLINE(lcTemp.MemoF,1),1,2)+'000'
lcDept    = SUBSTR(MLINE(lcTemp.MemoF,1),42,3)
lcOldPo   = SUBSTR(MLINE(lcTemp.MemoF,1),3,5)
lcEntered = SUBSTR(MLINE(lcTemp.MemoF,1),13,8)
ldEntered = CTOD(SUBSTR(lcEntered,5,2)+'/'+SUBSTR(lcEntered,7,2)+'/'+SUBSTR(lcEntered,1,4))
lcStart   = SUBSTR(MLINE(lcTemp.MemoF,1),21,8)
ldStart   = CTOD(SUBSTR(lcStart,5,2)+'/'+SUBSTR(lcStart,7,2)+'/'+SUBSTR(lcStart,1,4))
lcComplete= SUBSTR(MLINE(lcTemp.MemoF,1),29,8)
ldComplete= CTOD(SUBSTR(lcComplete,5,2)+'/'+SUBSTR(lcComplete,7,2)+'/'+SUBSTR(lcComplete,1,4))
lnBook    = 0
lnBookAmt = 0
lcStyle   = SPACE(19)
lnLineNo  = 0

IF !SEEK("M"+lcCust,'Customer')
  *-- Message : < The Customer  does not exist in the Customer file.>
  *-- Buttons : <                        OK                         >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Customer ' + lcCust+ ' does not exist in the Customer file.'  )  
  DO lfGoHome
  RETURN
ENDIF

DO WHILE lnNotLine <= lnMemLins
  lcStr   = MLINE(lcTemp.MemoF,lnNotLine)
  lcPo    = SUBSTR(lcStr,3,5)
  lcStore = SUBSTR(lcStr,8,5)
  lnOrdQty= VAL(SUBSTR(lcStr,72,4))
  lnPrice = VAL(SUBSTR(lcStr,76,6))/100
  lcUpc   = SUBSTR(lcStr,60,12)
  IF EMPTY(lcUpc)
    lnNotLine = lnNotLine + 1
    LOOP
  ENDIF
  IF lcPo <> lcOldPo
    llMulti = .T.
  ENDIF
  IF SEEK('S'+lcCust+lcUpc,'Spck_Lin')
    lcStyle = Spck_Lin.Style
    FOR I = 1 TO 8
      Z = STR(I,1)
      IF Spck_Lin.Qty&Z <> 0
        lcSiz = STR(I,1)
        EXIT
      ENDIF
    ENDFOR
    =SEEK(lcStyle,'Style')
    lcDiv   = Style.CDivision
    lcSea   = Style.Season
    lcDesc  = Style.Desc
    lcScale = Style.Scale
    IF llFirst
      lcOrder =GfSequence('ORDER','','',lcDiv,.F.)          
      llFirst = .F.
    ENDIF
    WAIT WINDOW ALLTRIM(lcStyle)+'/'+lcStore NOWAIT
  ELSE
    IF llFirst
      *-- Message : < UPC#  does not exist in the SKU file.>
      *-- Buttons : <                        OK                         >
      =gfModalGen("TRM000000B00000","DIALOG",'','','UPC# ' +lcUpc + ' does not exist in the SKU file.'  )  
      DO lfGoHome
      RETURN
    ENDIF
    *IF !llExcept
    *  llExcept = .T.
    *  lcSetTxt = SET("TEXTMERGE")
    *  SET TEXTMERGE ON NOSHOW
    *  SET TEXTMERGE DELIMITERS TO
    *  lcExcptDir = PUTFILE('Exceptional file','Except.TXT','TXT')
    *  IF EMPTY(lcExcptDir)
    *    *-- Message : < Operation has been cancelled >
    *    *-- Buttons : <             OK               >
    *    =gfModalGen("TRM000000B00000","DIALOG",'','','Operation has been cancelled.')  
    *  ELSE
    *    SET TEXTMERGE TO &lcExcptDir
    *    \ Error at Line
    *    \\ <<lnNotLine>>
    *    \\ The UPC No. 
    *    \\ <<lcUpc>>
    *    \\ Is not found in the spck_lin file.
    *    STORE _TEXT TO lcExcptHand
    *    STORE -1 TO _TEXT 
    *  ENDIF
    *ELSE
    *  IF !EMPTY(lcExcptDir)
    *    STORE lcExcptHand TO _TEXT
    *    \ Error at Line
    *    \\ <<lnNotLine>>
    *    \\ The UPC No. 
    *    \\ <<lcUpc>>
    *    \\ Is not found in the spck_lin file.
    *    STORE -1 To _TEXT   
	*  ENDIF
    *ENDIF
    lnNotLine = lnNotLine + 1
    LOOP
  ENDIF
  SELECT OrdLine
  IF !SEEK("O"+lcOrder+lcStore+SPACE(3)+lcStyle)
    lnLineNo = lnLineNo + 1            
    APPEND BLANK
    REPLACE CordType WITH "O"       ,;
            Order    WITH lcOrder   ,;
            Account  WITH lcCust    ,;
            LineNo   WITH lnLineNo  ,;
            Store    WITH lcStore   ,;
            CustPo   WITH lcPo      ,;
            Price    WITH lnPrice   ,;
            Start    WITH ldStart   ,;
            Complete WITH ldComplete,;
            Style    WITH lcStyle   ,;
            Scale    WITH lcScale   

  ENDIF
  REPLACE Qty&lcSiz WITH Qty&lcSiz + lnOrdQty         
  REPLACE TotQty    WITH TotQty + lnOrdQty
  lnBook    = lnBook + lnOrdQty
  lnBookAmt = lnBookAmt + lnOrdQty*lnPrice
  lnNotLine = lnNotLine + 1
ENDDO
=SEEK('M'+lcCust,'Customer')
SELECT OrdHdr
APPEND BLANK
REPLACE CordType    WITH "O"                    ,;
        Order       WITH lcOrder                ,;
        Account     WITH lcCust                 ,;
        Status      WITH 'O'                    ,;
        Dept        WITH lcDept                 ,;
        Priority    WITH '1'                    ,;
        CustPo      WITH IIF(!llMulti,lcPo,'')  ,;
        MultiPo     WITH llMulti                ,;
        Consol      WITH Customer.Consol        ,;
        Multi       WITH 'Y'                    ,;
        Season      WITH lcSea                  ,;
        CDivision   WITH lcDiv                  ,;
        CTermCode   WITH Customer.CTermCode     ,;  
        ShipVia     WITH Customer.ShipVia       ,;
        SpcInst     WITH Customer.SpcInst       ,;
        CInsur      WITH Customer.CInsur        ,;
        Buyer       WITH Customer.Buyer         ,;
        Phone       WITH Customer.Phone1        ,;
        CFacCode    WITH Customer.CFacCode      ,;
        FactAcct    WITH Customer.FactAcct      ,;
        Rep1        WITH Customer.SalesRep      ,;
        Comm1       WITH Customer.Comm          ,;
        Rep2        WITH Customer.Rep2          ,;
        Comm2       WITH Customer.Comm2         ,;
        Entered     WITH ldEntered              ,;
        Start       WITH ldStart                ,;
        Complete    WITH ldComplete             ,;
        Cancelled   WITH ldComplete             ,;
        Disc        WITH Customer.Disc          ,;
        Book        WITH lnBook                 ,;
        BookAmt     WITH lnBookAmt              ,;
        Open        WITH lnBook                 ,;
        OpenAmt     WITH lnBookAmt              ,;
        LastLine    WITH IIF(lnLineNo=1,lnLineNo,lnLineNo - 1) ,;
        Link_Code   WITH Customer.Link_Code     ,;
        DAdd_Date   WITH DATE()                 ,;
        CAdd_Time   WITH TIME()                 ,;
        cWareCode   WITH lcWareCode             ,;
        cAdd_User   WITH gcUser_Id
       
        
*IF llExcept
*  SET TEXTMERGE &lcSetTxt
*ENDIF

*IF !EMPTY(lcExcptDir)
*  =FCLOSE(lcExcptHand)
*ENDIF

WAIT CLEAR        
DO lfGoHome

RETURN 


*!*************************************************************
*! Name      : lfGoHome
*! Developer : Ahmed Mohamed EL_Anwar (AME)
*! Date      : 12/20/2000
*! Purpose   : To do necessary things to end program
*!*************************************************************
*! Example            : =lfGoHome(MemWidth)
*!*************************************************************
FUNCTION lfGoHome



SET MEMOWIDTH TO lnOldMemW

=gfCloseFile(GcDataDir+'Sequence')
=gfCloseFile(GcDataDir+'Style')
=gfCloseFile(GcDataDir+'Customer')
=gfCloseFile(GcDataDir+'OrdHdr')
=gfCloseFile(GcDataDir+'OrdLine')
=gfCloseFile(GcDataDir+'WareHous')
=gfCloseFile(GcDataDir+'Spck_Lin')

RETURN 