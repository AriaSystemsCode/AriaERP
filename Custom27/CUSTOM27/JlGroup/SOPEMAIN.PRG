*!**************************************************************************
*! Name      : SOPEMAIN.PRG
*! Developer : BASSEM RAFAAT
*! Date      : 05/30/2001
*! Purpose   : SOPE CREEK Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*B804492,1 SSE 10/22/2001 Fix bug of adding some modifications in Charges screen for customer J & L. 
*B804492,1                All modifications in SopeMain program.
*B606335,1 RAE 08/01/2002 Fix Spscript out of bounds bug.
*B606791,1 SSE 12/18/2002 Fix bug of shifting the custom bar for charges screen.
*B606157,1 ASH 02/03/2003 Let the user leave the hold reason empty.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*!*************************************************************
*! Name      : lfCHECK
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/30/2001
*! Purpose   : Update the temp file with the chargs.
*!*************************************************************
*! Called from  : SOPEMAIN.PRG
*! Program name : ARIINV.PRG (INVOICE SALES ORDER SCREEN)
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCHECK
*!*************************************************************
FUNCTION lfCHECK

*B804492,1 Add 10 fields for the Ratio popup (By invoice/By piece). [Begin]
*CREATE DBF (gcWorkDir+lcJLchargs) ;
*        (Invoice C(6)   , Order C(6)     , Account C(5)   , Store C(8)     , Piktkt C(6)     ,;
*         nChrg1 N(13,2) , nChrg2 N(13,2) , nChrg3 N(13,2) , nChrg4 N(13,2) , nChrg5 N(13,2)  ,;
*         nChrg6 N(13,2) , nChrg7 N(13,2) , nChrg8 N(13,2) , nChrg9 N(13,2) , nChrg10 N(13,2) ,;
*         consol C(1))
CREATE DBF (gcWorkDir+lcJLchargs) ;
        (Invoice C(6)   , Order C(6)     , Account C(5)   , Store C(8)     , Piktkt C(6)     ,;
         nChrg1 N(13,2) , nChrg2 N(13,2) , nChrg3 N(13,2) , nChrg4 N(13,2) , nChrg5 N(13,2)  ,;
         nChrg6 N(13,2) , nChrg7 N(13,2) , nChrg8 N(13,2) , nChrg9 N(13,2) , nChrg10 N(13,2) ,;
         consol C(1) , nRatio1 N(1) , nRatio2 N(1) , nRatio3 N(1) , nRatio4 N(1) , nRatio5 N(1) ,; 
         nRatio6 N(1) , nRatio7 N(1) , nRatio8 N(1) , nRatio9 N(1) , nRatio10 N(1) , ;
         nModChrg1 N(13,2) , nModChrg2 N(13,2) , nModChrg3 N(13,2) , nModChrg4 N(13,2) , ;
         nModChrg5 N(13,2) , nModChrg6 N(13,2) , nModChrg7 N(13,2) , nModChrg8 N(13,2) , ;
         nModChrg9 N(13,2) , nModChrg10 N(13,2))
*B804492,1 Add 10 fields for the Ratio popup (By invoice/By piece). [End]

INDEX ON ACCOUNT + ORDER + STORE + Piktkt TAG FULLINDEX OF (gcWorkDir+lcJLchargs)
INDEX ON ACCOUNT + STORE + CONSOL TAG CONSOLJL OF (gcWorkDir+lcJLchargs)
INDEX ON ORDER + ACCOUNT TAG ORDER OF (gcWorkDir+lcJLchargs)

*--End of lfCHECK.
*!*************************************************************
*! Name      : lfSUMALL
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/30/2001
*! Purpose   : 
*!*************************************************************
*! Called from  : SOPEMAIN.PRG
*! Program name : ARIINV.PRG (INVOICE SALES ORDER SCREEN)
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCHECK
*!*************************************************************
FUNCTION lfSUMALL
PRIVATE lcAlias , lcKey , lcOrder , lnRecNoS , lcAliasS

lcAlias = SELECT(0)
lnAllChrg = 0
llGetin2 = .T.

IF llGetin
  m.TotalChg = m.ShipAmt+m.Freight+m.Insur+m.Cod+m.Discount+m.Tax_Amt+m.nPstAmt+m.nHstAmt
ENDIF

SELECT (lcJlChargs)
lcOrder = ORDER()
SET ORDER TO TAG FULLINDEX

IF llGetin
  lcAliasS = SELECT(0)
  SELECT(lcInvHdr)
  lnRecNoS = RECNO(lcInvHdr)
  IF CONSOL = "Y"
    =SEEK(M.ACCOUNT)
    SCAN REST WHILE ACCOUNT + ORDER + STORE + PIKTKT = M.ACCOUNT FOR cSelect = '»' AND CONSOL = "N"
      IF !EMPTY(&lcInvHdr..ORDER)
        llGetin2 = .F.
         =SEEK(&lcInvHdr..ACCOUNT + &lcInvHdr..ORDER + &lcInvHdr..STORE + &lcInvHdr..PIKTKT , lcJlChargs)
         lnAllChrg = lnAllChrg + &lcJlChargs..nchrg1 + &lcJlChargs..nchrg2 + &lcJlChargs..nchrg3 + &lcJlChargs..nchrg4 + &lcJlChargs..nchrg5 +;
                                 &lcJlChargs..nchrg6 + &lcJlChargs..nchrg7 + &lcJlChargs..nchrg8 + &lcJlChargs..nchrg9 + &lcJlChargs..nchrg10
      ENDIF
    ENDSCAN
    SELECT (lcInvHdr)
    IF BETWEEN (lnRecNoS,1,RECCOUNT(lcInvHdr))
      GOTO lnRecNoS IN (lcInvHdr)
    ENDIF
    SELECT (lcAliasS)
  ENDIF
  llGetin = .F.
ENDIF

IF llGetin2
  =SEEK(&lcInvHdr..ACCOUNT + &lcInvHdr..ORDER + &lcInvHdr..STORE + &lcInvHdr..PIKTKT , lcJlChargs)
  lnAllChrg = &lcJlChargs..nchrg1 + &lcJlChargs..nchrg2 + &lcJlChargs..nchrg3 + &lcJlChargs..nchrg4 + &lcJlChargs..nchrg5 +;
              &lcJlChargs..nchrg6 + &lcJlChargs..nchrg7 + &lcJlChargs..nchrg8 + &lcJlChargs..nchrg9 + &lcJlChargs..nchrg10
ENDIF

IF m.TotalChg > 0
  m.TotalChg = ROUND(m.TotalChg + lnAllChrg ,2)
ENDIF

SELECT (lcJlChargs)
SET ORDER TO TAG &lcOrder

SELECT(lcAlias)

*--End of lfSUMALL.
*!*************************************************************
*! Name      : lfUPDATE
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Update the custome dbf JLCHARGS.DBF with the charges.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfUPDATE()
*!*************************************************************
FUNCTION lfUPDATE
PRIVATE lcAlias , lckeyInv , lcKeyOrdr
STORE .F. TO llSetRel
STORE 0 TO lnConYSt1 , lnConYSt2 , lnConYSt3 , lnConYSt4 , lnConYSt5
STORE 0 TO lnConYSt6 , lnConYSt7 , lnConYSt8 , lnConYSt9 , lnConYSt10
lnLastTme = 2        && Check if the scan end with no update for the charges for the last account.

lcAlias = SELECT(0)
SELECT ORDHDR
lcKeyOrdr = EVAL(KEY())

SELECT(lcInvHdr)
lckeyInv = EVAL(KEY())

IF !('ORDER INTO ORDHDR' $ SET('RELATION'))
  SET RELATION TO "O" + ORDER INTO ORDHDR ADDITIVE
  llSetRel = .T.
ENDIF

STORE SPACE(0) TO lcAccount , lcAccnt2

SCAN
  SCATTER MEMVAR MEMO
  SELECT (lcJLchargs)

  *--This case if there is more than one account and in the first record for the seconed account I get in to update the firts one.
  IF lcAccnt2 = M.ACCOUNT AND !EMPTY(lcAccnt2) AND M.CONSOL = "Y"
    lnTotConSt = lnConYSt1 + lnConYSt2 + lnConYSt3 + lnConYSt4 + lnConYSt5 +;
                 lnConYSt6 + lnConYSt7 + lnConYSt8 + lnConYSt9 + lnConYSt10
    IF lnTotConSt > 0
      lckeyCon = ORDER()
      SET ORDER TO TAG CONSOLJL
      IF SEEK(lcAccnt2 + SPACE(8) + "Y", lcJLchargs)
        REPLACE &lcJLchargs..NCHRG1   WITH lnConYSt1  ,;
                &lcJLchargs..NCHRG2   WITH lnConYSt2  ,;
                &lcJLchargs..NCHRG3   WITH lnConYSt3  ,;
                &lcJLchargs..NCHRG4   WITH lnConYSt4  ,;
                &lcJLchargs..NCHRG5   WITH lnConYSt5  ,;
                &lcJLchargs..NCHRG6   WITH lnConYSt6  ,;
                &lcJLchargs..NCHRG7   WITH lnConYSt7  ,;
                &lcJLchargs..NCHRG8   WITH lnConYSt8  ,;
                &lcJLchargs..NCHRG9   WITH lnConYSt9  ,;
                &lcJLchargs..NCHRG10  WITH lnConYSt10
      lnLastTme = 2
      ENDIF
      SET ORDER TO TAG &lckeyCon
    ENDIF
  ENDIF

  APPEN BLANK
  IF M.CONSOL = "Y" AND EMPTY(M.ORDER)
    lcAccount = M.ACCOUNT
    GATHER MEMVAR MEMO
  ELSE
    IF M.CONSOL = "Y" AND EMPTY(M.STORE)
      lcAccnt2 = M.ACCOUNT
      lnLastTme = 1
      STORE 0 TO lnConYSt1 , lnConYSt2 , lnConYSt3 , lnConYSt4 , lnConYSt5
      STORE 0 TO lnConYSt6 , lnConYSt7 , lnConYSt8 , lnConYSt9 , lnConYSt10
    ENDIF
    IF lcAccnt2 = M.ACCOUNT AND !EMPTY(M.STORE)
      lnConYSt1  = lnConYSt1  + ORDHDR.NCHRG1
      lnConYSt2  = lnConYSt2  + ORDHDR.NCHRG2
      lnConYSt3  = lnConYSt3  + ORDHDR.NCHRG3
      lnConYSt4  = lnConYSt4  + ORDHDR.NCHRG4
      lnConYSt5  = lnConYSt5  + ORDHDR.NCHRG5
      lnConYSt6  = lnConYSt6  + ORDHDR.NCHRG6
      lnConYSt7  = lnConYSt7  + ORDHDR.NCHRG7
      lnConYSt8  = lnConYSt8  + ORDHDR.NCHRG8
      lnConYSt9  = lnConYSt9  + ORDHDR.NCHRG9
      lnConYSt10 = lnConYSt10 + ORDHDR.NCHRG10
    
    ENDIF

    *B804492,1 Update the nChrg1...10. [Begin]
    *REPLACE &lcJLchargs..ORDER    WITH M.ORDER        ,;
    *        &lcJLchargs..STORE    WITH M.STORE        ,;
    *        &lcJLchargs..Account  WITH M.ACCOUNT      ,;
    *        &lcJLchargs..Piktkt   WITH M.Piktkt       ,;
    *        &lcJLchargs..Consol   WITH M.CONSOL       ,;
    *        &lcJLchargs..NCHRG1   WITH ORDHDR.NCHRG1  ,;
    *        &lcJLchargs..NCHRG2   WITH ORDHDR.NCHRG2  ,;
    *        &lcJLchargs..NCHRG3   WITH ORDHDR.NCHRG3  ,;
    *        &lcJLchargs..NCHRG4   WITH ORDHDR.NCHRG4  ,;
    *        &lcJLchargs..NCHRG5   WITH ORDHDR.NCHRG5  ,;
    *        &lcJLchargs..NCHRG6   WITH ORDHDR.NCHRG6  ,;
    *        &lcJLchargs..NCHRG7   WITH ORDHDR.NCHRG7  ,;
    *        &lcJLchargs..NCHRG8   WITH ORDHDR.NCHRG8  ,;
    *        &lcJLchargs..NCHRG9   WITH ORDHDR.NCHRG9  ,;
    *        &lcJLchargs..NCHRG10  WITH ORDHDR.NCHRG10    
    REPLACE &lcJLchargs..ORDER    WITH M.ORDER        ,;
            &lcJLchargs..STORE    WITH M.STORE        ,;
            &lcJLchargs..Account  WITH M.ACCOUNT      ,;
            &lcJLchargs..Piktkt   WITH M.Piktkt       ,;
            &lcJLchargs..Consol   WITH M.CONSOL
            
    REPLACE &lcJLchargs..NCHRG1   WITH ORDHDR.NCHRG1 * IIF(&lcJLchargs..nRatio1=1,1,m.Ship),;
            &lcJLchargs..NCHRG2   WITH ORDHDR.NCHRG2 * IIF(&lcJLchargs..nRatio2=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG3   WITH ORDHDR.NCHRG3 * IIF(&lcJLchargs..nRatio3=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG4   WITH ORDHDR.NCHRG4 * IIF(&lcJLchargs..nRatio4=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG5   WITH ORDHDR.NCHRG5 * IIF(&lcJLchargs..nRatio5=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG6   WITH ORDHDR.NCHRG6 * IIF(&lcJLchargs..nRatio6=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG7   WITH ORDHDR.NCHRG7 * IIF(&lcJLchargs..nRatio7=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG8   WITH ORDHDR.NCHRG8 * IIF(&lcJLchargs..nRatio8=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG9   WITH ORDHDR.NCHRG9 * IIF(&lcJLchargs..nRatio9=1,1,m.Ship) ,;
            &lcJLchargs..NCHRG10  WITH ORDHDR.NCHRG10 * IIF(&lcJLchargs..nRatio10=1,1,m.Ship)
    *B804492,1 Update the nChrg1...10. [End]

    *B804492,1 Update the nModChrg1...10. [Begin]
    REPLACE &lcJLchargs..nModChrg1   WITH OrdHdr.nChrg1  ,;
            &lcJLchargs..nModChrg2   WITH OrdHdr.nChrg2  ,;
            &lcJLchargs..nModChrg3   WITH OrdHdr.nChrg3  ,;
            &lcJLchargs..nModChrg4   WITH OrdHdr.nChrg4  ,;
            &lcJLchargs..nModChrg5   WITH OrdHdr.nChrg5  ,;
            &lcJLchargs..nModChrg6   WITH OrdHdr.nChrg6  ,;
            &lcJLchargs..nModChrg7   WITH OrdHdr.nChrg7  ,;
            &lcJLchargs..nModChrg8   WITH OrdHdr.nChrg8  ,;
            &lcJLchargs..nModChrg9   WITH OrdHdr.nChrg9  ,;
            &lcJLchargs..nModChrg10  WITH OrdHdr.nChrg10    
    *B804492,1 Update the nModChrg1...10. [End]
    
    IF lcAccount = M.ACCOUNT
      IF SEEK(SPACE(6) + M.ACCOUNT , lcJLchargs )
      
        *B804492,1 Update the charge fields according to the default BY PIECE. [Begin]
        *REPLACE &lcJLchargs..NCHRG1   WITH &lcJLchargs..NCHRG1  + ORDHDR.NCHRG1  ,;
        *        &lcJLchargs..NCHRG2   WITH &lcJLchargs..NCHRG2  + ORDHDR.NCHRG2  ,;
        *        &lcJLchargs..NCHRG3   WITH &lcJLchargs..NCHRG3  + ORDHDR.NCHRG3  ,;
        *        &lcJLchargs..NCHRG4   WITH &lcJLchargs..NCHRG4  + ORDHDR.NCHRG4  ,;
        *        &lcJLchargs..NCHRG5   WITH &lcJLchargs..NCHRG5  + ORDHDR.NCHRG5  ,;
        *        &lcJLchargs..NCHRG6   WITH &lcJLchargs..NCHRG6  + ORDHDR.NCHRG6  ,;
        *        &lcJLchargs..NCHRG7   WITH &lcJLchargs..NCHRG7  + ORDHDR.NCHRG7  ,;
        *        &lcJLchargs..NCHRG8   WITH &lcJLchargs..NCHRG8  + ORDHDR.NCHRG8  ,;
        *        &lcJLchargs..NCHRG9   WITH &lcJLchargs..NCHRG9  + ORDHDR.NCHRG9  ,;
        *        &lcJLchargs..NCHRG10  WITH &lcJLchargs..NCHRG10 + ORDHDR.NCHRG10
        REPLACE &lcJLchargs..NCHRG1   WITH &lcJLchargs..NCHRG1  + (ORDHDR.NCHRG1 * IIF(&lcJLchargs..nRatio1=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG2   WITH &lcJLchargs..NCHRG2  + (ORDHDR.NCHRG2 * IIF(&lcJLchargs..nRatio2=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG3   WITH &lcJLchargs..NCHRG3  + (ORDHDR.NCHRG3 * IIF(&lcJLchargs..nRatio3=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG4   WITH &lcJLchargs..NCHRG4  + (ORDHDR.NCHRG4 * IIF(&lcJLchargs..nRatio4=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG5   WITH &lcJLchargs..NCHRG5  + (ORDHDR.NCHRG5 * IIF(&lcJLchargs..nRatio5=1,1,m.Ship))
                
        REPLACE &lcJLchargs..NCHRG6   WITH &lcJLchargs..NCHRG6  + (ORDHDR.NCHRG6 * IIF(&lcJLchargs..nRatio6=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG7   WITH &lcJLchargs..NCHRG7  + (ORDHDR.NCHRG7 * IIF(&lcJLchargs..nRatio7=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG8   WITH &lcJLchargs..NCHRG8  + (ORDHDR.NCHRG8 * IIF(&lcJLchargs..nRatio8=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG9   WITH &lcJLchargs..NCHRG9  + (ORDHDR.NCHRG9 * IIF(&lcJLchargs..nRatio9=1,1,m.Ship)) ,;
                &lcJLchargs..NCHRG10  WITH &lcJLchargs..NCHRG10 + (ORDHDR.NCHRG10 * IIF(&lcJLchargs..nRatio10=1,1,m.Ship))
        *B804492,1 Update the charge fields according to the default BY PIECE. [End]

        *B804492,1 Update the nModChrg1...10. [Begin]
        REPLACE &lcJLchargs..nModChrg1   WITH &lcJLchargs..nModChrg1 + OrdHdr.nChrg1  ,;
                &lcJLchargs..nModChrg2   WITH &lcJLchargs..nModChrg2 + OrdHdr.nChrg2  ,;
                &lcJLchargs..nModChrg3   WITH &lcJLchargs..nModChrg3 + OrdHdr.nChrg3  ,;
                &lcJLchargs..nModChrg4   WITH &lcJLchargs..nModChrg4 + OrdHdr.nChrg4  ,;
                &lcJLchargs..nModChrg5   WITH &lcJLchargs..nModChrg5 + OrdHdr.nChrg5  ,;
                &lcJLchargs..nModChrg6   WITH &lcJLchargs..nModChrg6 + OrdHdr.nChrg6  ,;
                &lcJLchargs..nModChrg7   WITH &lcJLchargs..nModChrg7 + OrdHdr.nChrg7  ,;
                &lcJLchargs..nModChrg8   WITH &lcJLchargs..nModChrg8 + OrdHdr.nChrg8  ,;
                &lcJLchargs..nModChrg9   WITH &lcJLchargs..nModChrg9 + OrdHdr.nChrg9  ,;
                &lcJLchargs..nModChrg10  WITH &lcJLchargs..nModChrg10 + OrdHdr.nChrg10    
        *B804492,1 Update the nModChrg1...10. [End]

      ENDIF
    ENDIF
    lcAccount = M.ACCOUNT
  ENDIF
ENDSCAN

IF lnLastTme = 1
  SELECT (lcJLchargs)
  lckeyCon = ORDER()
  SET ORDER TO TAG CONSOLJL
  IF SEEK(lcAccnt2 + SPACE(8) + "Y", lcJLchargs)
    REPLACE &lcJLchargs..NCHRG1   WITH lnConYSt1  ,;
            &lcJLchargs..NCHRG2   WITH lnConYSt2  ,;
            &lcJLchargs..NCHRG3   WITH lnConYSt3  ,;
            &lcJLchargs..NCHRG4   WITH lnConYSt4  ,;
            &lcJLchargs..NCHRG5   WITH lnConYSt5  ,;
            &lcJLchargs..NCHRG6   WITH lnConYSt6  ,;
            &lcJLchargs..NCHRG7   WITH lnConYSt7  ,;
            &lcJLchargs..NCHRG8   WITH lnConYSt8  ,;
            &lcJLchargs..NCHRG9   WITH lnConYSt9  ,;
            &lcJLchargs..NCHRG10  WITH lnConYSt10
  ENDIF
  SET ORDER TO TAG &lckeyCon
ENDIF

IF llSetRel
  SELECT(lcInvHdr)
  SET RELATION OFF INTO ORDHDR
ENDIF

=SEEK(lcKeyOrdr,'ORDHDR')
=SEEK(lckeyInv,lcInvHdr)

SELECT(lcAlias)

*--End of lfUPDATE.
*!*************************************************************
*! Name      : lfChargScr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Show the charges screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfChargScr()
*!*************************************************************
FUNCTION lfChargScr
PRIVATE lcAlias , lcKey , lcOrder , lcAlias2 , lnRecNoS

*B804492 Initialize the popup variables. [Begin]
STORE 2 TO lnRatio1 , lnRatio2 , lnRatio3 , lnRatio4 , lnRatio5 , ;
           lnRatio6 , lnRatio7 , lnRatio8 , lnRatio9 , lnRatio10
*B804492 Initialize the popup variables. [End]

llConsol2 = .T.
lcAlias = SELECT(0)
IF !USED('SYDFIELD')
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'CFLD_NAME','SH')
ENDIF

FOR lnNo = 1 TO 10
  lcNo = ALLTRIM(STR(lnNo))
  =SEEK('NCHRG' + lcNo , 'SYDFIELD')
  lccharge&lcNo = ALLTRIM(SYDFIELD.CFLD_HEAD)
ENDFOR

SELECT (lcJlChargs)
lcOrder = ORDER()
SET ORDER TO TAG FULLINDEX

SELECT (lcInvHdr)

IF &lcInvHdr..CONSOL = "Y"
  lcObjChrg = "DISABLE"
  llHide = .F.
ELSE
  lcObjChrg = "ENABLE"
  llHide = .T.
ENDIF

lcAlias2 = SELECT(0)
SELECT(lcInvHdr)

*B804492,1 Save current account code. [Begin]
lcCurAccnt = &lcInvHdr..Account
*B804492,1 Save current account code. [End]

lnRecNoS = RECNO(lcInvHdr)
IF CONSOL = "Y"
  
  *B804492,1 Initialize m.nModChrg1...10. [Begin]
  *=SEEK(M.ACCOUNT)
  STORE 0 TO m.nModChrg1 , m.nModChrg2 , m.nModChrg3 , m.nModChrg4 , m.nModChrg5 , ;
             m.nModChrg6 , m.nModChrg7 , m.nModChrg8 , m.nModChrg9 , m.nModChrg1
  *B804492,1 Initialize m.nModChrg1...10. [End]

  STORE 0 TO M.NCHRG1,M.NCHRG2,M.NCHRG3,M.NCHRG4,M.NCHRG5,M.NCHRG6,M.NCHRG7,M.NCHRG8,M.NCHRG9,M.NCHRG10

  *B804492,1 scan on saved account code. [Begin]
  *SCAN REST WHILE ACCOUNT + ORDER + STORE + PIKTKT = M.ACCOUNT FOR cSelect = '»' AND CONSOL = "N"
  SCAN REST WHILE ACCOUNT + ORDER + STORE + PIKTKT = lcCurAccnt FOR cSelect = '»' AND CONSOL = "N"
  *B804492,1 scan on saved account code. [End]
  
    IF !EMPTY(&lcInvHdr..ORDER)
       llConsol2 = .F.
       =SEEK(&lcInvHdr..ACCOUNT + &lcInvHdr..ORDER + &lcInvHdr..STORE + &lcInvHdr..PIKTKT , lcJlChargs)
       FOR lnNo = 1 TO 10
         lcNo = ALLTRIM(STR(lnNo))
         M.NCHRG&lcNo = M.NCHRG&lcNo + &lcJLchargs..NCHRG&lcNo

         *B804492,1 Update the new fields nModChrg1...10. [Begin]
         m.nModChrg&lcNo = m.nChrg&lcNo + &lcJLchargs..nChrg&lcNo
         *B804492,1 Update the new fields nModChrg1...10. [End]
       ENDFOR
    ENDIF
  ENDSCAN
  SELECT (lcInvHdr)
  IF BETWEEN (lnRecNoS,1,RECCOUNT(lcInvHdr))
    GOTO lnRecNoS IN (lcInvHdr)
  ENDIF
  SELECT (lcAlias2)
ENDIF

IF llConsol2
  =SEEK(&lcInvHdr..ACCOUNT + &lcInvHdr..ORDER + &lcInvHdr..STORE + &lcInvHdr..PIKTKT , lcJlChargs)
  FOR lnNo = 1 TO 10
    lcNo = ALLTRIM(STR(lnNo))

    *B804492,1 Get the charges whether by Invoice or Pieces. [Begin]
    *M.NCHRG&lcNo = &lcJLchargs..NCHRG&lcNo
    m.NCHRG&lcNo = IIF(&lcJLchargs..nRatio&lcNo = 1,&lcJLchargs..nChrg&lcNo,&lcJLchargs..nChrg&lcNo/&lcInvHdr..Ship)
    m.nModChrg&lcNo = &lcJLchargs..nModChrg&lcNo
    lnRatio&lcNo = IIF(&lcJLchargs..nRatio&lcNo=0,2,&lcJLchargs..nRatio&lcNo)
    *B804492,1 Get the charges whether by Invoice or Pieces. [Begin]

  ENDFOR
ENDIF

*B804492,1 Run either one of the 2 screens. [Begin]
*DO (gcScrDir+gcWinAppl+"\JlChargs.SPR")
IF llHide
  DO (gcScrDir+gcWinAppl+"\JlChargs.SPR")
ELSe
  DO (gcScrDir+gcWinAppl+"\JlCharg1.SPR")
ENDIF  
*B804492,1 Run either one of the 2 screens. [End]

SELECT (lcJlChargs)
SET ORDER TO TAG &lcOrder

*B804492,1 Add the commented lcAlias below in this function instead of the next one. [Begin]
SELECT (lcAlias)
*B804492,1 Add the commented lcAlias below in this function instead of the next one. [End]

*--The validation of the OK bottun which in the same time saving in the 
*--Temp. file in case the user change the chargs of the selected order.
FUNCTION lfSaveOk
PRIVATE lcFile
lcFile = SELECT(0)

SELECT (lcJLchargs)
=SEEK(&lcInvHdr..ACCOUNT + &lcInvHdr..ORDER + &lcInvHdr..STORE + &lcInvHdr..PIKTKT , lcJlChargs)
lnOldChrgs = nchrg1 + nchrg2 + nchrg3 + nchrg4 + nchrg5 + nchrg6 + nchrg7 + nchrg8 + nchrg9 + nchrg10

*B804492,1 Save the Ratio type whether by invoice or piece. [Begin]
*REPLACE nchrg1  WITH M.NCHRG1 ,;
*        nchrg2  WITH M.NCHRG2 ,;
*        nchrg3  WITH M.NCHRG3 ,;
*        nchrg4  WITH M.NCHRG4 ,;
*        nchrg5  WITH M.NCHRG5 ,;
*        nchrg6  WITH M.NCHRG6 ,;
*        nchrg7  WITH M.NCHRG7 ,;
*        nchrg8  WITH M.NCHRG8 ,;
*        nchrg9  WITH M.NCHRG9 ,;
*        nchrg10 WITH M.NCHRG10
REPLACE nchrg1  WITH m.nModChrg1 * IIF(lnRatio1=1,1,&lcInvHdr..Ship),;
        nchrg2  WITH m.nModChrg2 * IIF(lnRatio2=1,1,&lcInvHdr..Ship),;
        nchrg3  WITH m.nModChrg3 * IIF(lnRatio3=1,1,&lcInvHdr..Ship),;
        nchrg4  WITH m.nModChrg4 * IIF(lnRatio4=1,1,&lcInvHdr..Ship),;
        nchrg5  WITH m.nModChrg5 * IIF(lnRatio5=1,1,&lcInvHdr..Ship),;
        nchrg6  WITH m.nModChrg6 * IIF(lnRatio6=1,1,&lcInvHdr..Ship),;
        nchrg7  WITH m.nModChrg7 * IIF(lnRatio7=1,1,&lcInvHdr..Ship),;
        nchrg8  WITH m.nModChrg8 * IIF(lnRatio8=1,1,&lcInvHdr..Ship),;
        nchrg9  WITH m.nModChrg9 * IIF(lnRatio9=1,1,&lcInvHdr..Ship),;
        nchrg10 WITH m.nModChrg10 * IIF(lnRatio10=1,1,&lcInvHdr..Ship),;
        nRatio1  WITH lnRatio1 ,;
        nRatio2  WITH lnRatio2 ,;
        nRatio3  WITH lnRatio3 ,;
        nRatio4  WITH lnRatio4 ,;
        nRatio5  WITH lnRatio5 ,;
        nRatio6  WITH lnRatio6 ,;
        nRatio7  WITH lnRatio7 ,;
        nRatio8  WITH lnRatio8 ,;
        nRatio9  WITH lnRatio9 ,;
        nRatio10 WITH lnRatio10
         
REPLACE nModchrg1  WITH m.nModChrg1  ,;
        nModchrg2  WITH m.nModChrg2  ,;
        nModchrg3  WITH m.nModChrg3  ,;
        nModchrg4  WITH m.nModChrg4  ,;
        nModchrg5  WITH m.nModChrg5  ,;
        nModchrg6  WITH m.nModChrg6  ,;
        nModchrg7  WITH m.nModChrg7  ,;
        nModchrg8  WITH m.nModChrg8  ,;
        nModchrg9  WITH m.nModChrg9  ,;
        nModchrg10 WITH m.nModChrg10        
*B804492,1 Save the Ratio type whether by invoice or piece. [End]

*--Case the user modify the charges in the charges folder.
lnNewChrgs = nchrg1 + nchrg2 + nchrg3 + nchrg4 + nchrg5 + nchrg6 + nchrg7 + nchrg8 + nchrg9 + nchrg10
llChangJL = IIF(lnNewChrgs # lnOldChrgs , .T. , .F.)
IF llChangJL
  M.TOTALCHG = M.TOTALCHG + (lnNewChrgs - lnOldChrgs)
ENDIF

IF SEEK('M' +  &lcJLchargs..Account , 'CUSTOMER')
  lcAccount = ""
  lcAccount = &lcJLchargs..Account
  IF CUSTOMER.CONSOL = "Y"
    SELECT (lcJLchargs)
    lcKey = ORDER()
    SET ORDER TO TAG ORDER
    STORE 0 TO LnNCHRG1,LnNCHRG2,LnNCHRG3,LnNCHRG4,LnNCHRG5,LnNCHRG6,LnNCHRG7,LnNCHRG8,LnNCHRG9,LnNCHRG10

    IF SEEK(SPACE(6) + lcAccount , lcJLchargs)
      SCAN REST WHILE ORDER + ACCOUNT = ""  FOR !EMPTY(ORDER) AND &lcJLchargs..Account = lcAccount
        FOR lnNo = 1 TO 10
          lcNo = ALLTRIM(STR(lnNo))
          LnNCHRG&lcNo = LnNCHRG&lcNo + &lcJLchargs..NCHRG&lcNo
        ENDFOR
      ENDSCAN

      IF SEEK(SPACE(6) + lcAccount , lcJLchargs )
        REPLACE &lcJLchargs..NCHRG1   WITH LnNCHRG1  ,;
                &lcJLchargs..NCHRG2   WITH LnNCHRG2  ,;
                &lcJLchargs..NCHRG3   WITH LnNCHRG3  ,;
                &lcJLchargs..NCHRG4   WITH LnNCHRG4  ,;
                &lcJLchargs..NCHRG5   WITH LnNCHRG5  ,;
                &lcJLchargs..NCHRG6   WITH LnNCHRG6  ,;
                &lcJLchargs..NCHRG7   WITH LnNCHRG7  ,;
                &lcJLchargs..NCHRG8   WITH LnNCHRG8  ,;
                &lcJLchargs..NCHRG9   WITH LnNCHRG9  ,;
                &lcJLchargs..NCHRG10  WITH LnNCHRG10
      ENDIF
    ENDIF

    SET ORDER TO TAG CONSOLJL
    IF SEEK(lcAccount + SPACE(8) + "Y", lcJLchargs)
      SCAN REST WHILE ACCOUNT + STORE + CONSOL = ""  FOR  CONSOL = "N" AND &lcJLchargs..Account = lcAccount
        FOR lnNo = 1 TO 10
          lcNo = ALLTRIM(STR(lnNo))
          LnNCHRG&lcNo = LnNCHRG&lcNo + &lcJLchargs..NCHRG&lcNo
        ENDFOR
      ENDSCAN

      IF SEEK(lcAccount + SPACE(8) + "Y", lcJLchargs)
        REPLACE &lcJLchargs..NCHRG1   WITH LnNCHRG1  ,;
                &lcJLchargs..NCHRG2   WITH LnNCHRG2  ,;
                &lcJLchargs..NCHRG3   WITH LnNCHRG3  ,;
                &lcJLchargs..NCHRG4   WITH LnNCHRG4  ,;
                &lcJLchargs..NCHRG5   WITH LnNCHRG5  ,;
                &lcJLchargs..NCHRG6   WITH LnNCHRG6  ,;
                &lcJLchargs..NCHRG7   WITH LnNCHRG7  ,;
                &lcJLchargs..NCHRG8   WITH LnNCHRG8  ,;
                &lcJLchargs..NCHRG9   WITH LnNCHRG9  ,;
                &lcJLchargs..NCHRG10  WITH LnNCHRG10
      ENDIF
    ENDIF
    SELECT (lcJLchargs)
    SET ORDER TO TAG &lcKey
  ENDIF
ENDIF


SELECT(lcFile)

*--End of lfSaveOk.

*B804492,1 SSE Commented out. [Begin]
*SELECT (lcAlias)
*B804492,1 SSE Commented out. [End]

*--End of lfChargScr.
*!*************************************************************
*! Name      : lfJLInsert
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/30/2001
*! Purpose   : Update the temp file with the chargs.
*!*************************************************************
*! Called from  : SOPEMAIN.PRG
*! Program name : ARIINV.PRG (INVOICE SALES ORDER SCREEN)
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCHECK
*!*************************************************************
FUNCTION lfJLInsert
PRIVATE lcAlias , lnTotal , lcOrder

lcAlias = SELECT(0)
lnTotal = 0

SELECT (lcJLchargs)
lcOrder = ORDER()
SET ORDER TO TAG FULLINDEX

SELECT (lcHdrFile)
SET RELATION TO ACCOUNT + ORDER + STORE + Piktkt INTO (lcJLchargs) ADDITIVE

IF !USED('JLCHARGS')
  =gfOpenFile(gcDataDir+'JLCHARGS',gcDataDir+'INVOICE','SH')
ENDIF

SELECT JLCHARGS
INSERT INTO JLCHARGS FROM MEMVAR
lnTotal = &lcJLchargs..NCHRG1 + &lcJLchargs..NCHRG2 + &lcJLchargs..NCHRG3 + &lcJLchargs..NCHRG4 + &lcJLchargs..NCHRG5 +;
          &lcJLchargs..NCHRG6 + &lcJLchargs..NCHRG7 + &lcJLchargs..NCHRG8 + &lcJLchargs..NCHRG9 + &lcJLchargs..NCHRG10

REPLACE NCHRG1   WITH &lcJLchargs..NCHRG1  ,;
        NCHRG2   WITH &lcJLchargs..NCHRG2  ,;
        NCHRG3   WITH &lcJLchargs..NCHRG3  ,;
        NCHRG4   WITH &lcJLchargs..NCHRG4  ,;
        NCHRG5   WITH &lcJLchargs..NCHRG5  ,;
        NCHRG6   WITH &lcJLchargs..NCHRG6  ,;
        NCHRG7   WITH &lcJLchargs..NCHRG7  ,;
        NCHRG8   WITH &lcJLchargs..NCHRG8  ,;
        NCHRG9   WITH &lcJLchargs..NCHRG9  ,;
        NCHRG10  WITH &lcJLchargs..NCHRG10

SELECT (lcHdrFile)
SET RELATION OFF INTO (lcJLchargs)

SELECT (lcJLchargs)
SET ORDER TO TAG &lcOrder

SELECT(lcAlias)

*--End of lfJLInsert.
*!*************************************************************
*! Name      : lfDINVCHRG
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Show the charges screen from the direct invoice.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =lfDINVCHRG()
*!*************************************************************
FUNCTION lfDINVCHRG
PRIVATE lcAlias

lcAlias = SELECT(0)
IF !USED('SYDFIELD')
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'CFLD_NAME','SH')
ENDIF

FOR lnNo = 1 TO 10
  lcNo = ALLTRIM(STR(lnNo))
  =SEEK('NCHRG' + lcNo , 'SYDFIELD')
  lccharge&lcNo = ALLTRIM(SYDFIELD.CFLD_HEAD)
ENDFOR

IF !USED('JLCHARGS')
  =gfOpenFile(gcDataDir+'JLCHARGS',gcDataDir+'INVOICE','SH')
ENDIF

SELECT INVHDR
=SEEK(INVHDR.INVOICE , 'JLCHARGS')

lcObjChrg = "DISABLE"
llHide = .F.

FOR lnNo = 1 TO 10
  lcNo = ALLTRIM(STR(lnNo))
  M.NCHRG&lcNo = JLCHARGS.NCHRG&lcNo
ENDFOR

*B804492,1 Call the new charges screen. [Begin]
*DO (gcScrDir+gcWinAppl+"\JlChargs.SPR")
DO (gcScrDir+gcWinAppl+"\JlCharg1.SPR")
*B804492,1 Call the new charges screen. [End]

SELECT(lcAlias)

*--End of lfDINVCHRG.
*!*************************************************************
*! Name      : LFFOLDER
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Add the charges in the option folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =LFFOLDER()
*!*************************************************************
FUNCTION LFFOLDER

*B606791,1 Shift the bar [Begin]
*DEFINE BAR 14 OF _INQURYPOP PROMPT 'Charg\<es '  SKIP FOR laScrMode[1]
*DEFINE BAR 15 OF _INQURYPOP PROMPT 'Charg\<es '  SKIP FOR laScrMode[1]
lnBarNo = CNTBAR('_INQURYPOP') + 1
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Charg\<es '  SKIP FOR laScrMode[1]
ON SELECTION BAR lnBarNo   OF _INQURYPOP DO lfChargScr IN SOPEMAIN.PRG
*B606791,1 Shift the bar [End]

*--End of LFFOLDER.
*!*************************************************************
*! Name      : LFFOLDERINV
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Add the charges in the option folder.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =LFFOLDERINV()
*!*************************************************************
FUNCTION LFFOLDERINV

DEFINE BAR 13 OF _INQURYPOP PROMPT 'Charg\<es '

*--End of LFFOLDER.
*!*************************************************************
*! Name      : LFREMOVE
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Remove the temp. file after closing the screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =LFREMOVE()
*!*************************************************************
FUNCTION LFREMOVE

IF USED(lcJLchargs)
  USE IN (lcJLchargs)
ENDIF
ERASE (gcWorkDir+lcJLchargs+".DBF")
ERASE (gcWorkDir+lcJLchargs+".CDX")
ERASE (gcWorkDir+lcJLchargs+".FPT")


*--End of LFREMOVE.
*!*************************************************************
*! Name      : LFUPDATCHG
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Update the LCINVHDR with the total chargs.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =LFUPDATCHG()
*!*************************************************************
FUNCTION LFUPDATCHG
PRIVATE lcOrder , lcAlias

lcAlias = SELECT(0)
SELECT (lcJLchargs)
lcOrder = ORDER()
SET ORDER TO TAG FULLINDEX

SELECT (lcInvHdr)
SET RELATION TO ACCOUNT + ORDER + STORE + Piktkt INTO (lcJLchargs) ADDITIVE

SCAN
   IF &lcInvHdr..TOTALCHG <> 0
     REPLACE TOTALCHG WITH TOTALCHG + &lcJLchargs..NCHRG1 + &lcJLchargs..NCHRG2 + &lcJLchargs..NCHRG3 + &lcJLchargs..NCHRG4 +;
                                      &lcJLchargs..NCHRG5 + &lcJLchargs..NCHRG6 + &lcJLchargs..NCHRG7 + &lcJLchargs..NCHRG8 +;
                                      &lcJLchargs..NCHRG9 + &lcJLchargs..NCHRG10
  ENDIF
ENDSCAN

SELECT (lcInvHdr)
SET RELATION OFF INTO (lcJLchargs)

SELECT (lcJLchargs)
SET ORDER TO TAG &lcOrder

SELECT(lcAlias)

*--End of LFUPDATCHG.
*!*************************************************************
*! Name      : LFDELETE
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Zap the LCJLCHARGS file when closing the screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   :  =LFDELETE()
*!*************************************************************
FUNCTION LFDELETE

IF USED(lcJLchargs)
  SELECT (lcJLchargs)
  ZAP
ENDIF

*--End of LFUPDATCHG.

*!*************************************************************
*! Name      : LFSONVVAL
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Call this function from the SYDFIELD.DBF for 
*!           : the fields in the sales order screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   : =LFSONVVAL()
*!*************************************************************
FUNCTION LFSONVVAL
PARAMETERS lcReturn , lcFiled
lcReturn = .T.

lcFiled = ALLTRIM(STR(lcFiled))
IF TYPE('LAOGFXFLT') # 'U'
  lnDataPos = ASCAN(laogFxflt,'NCHRG'+lcFiled)
  IF lnDataPos > 0
    lnDataPos = ASUBSCRIPT(laogFxflt,lnDataPos,1)
    lnOldChrg = laogFxflt[lnDataPos,6]
  ENDIF
  **Ash 08/01/2001
  IF TYPE('lnOldChrg') = 'U'
    RETURN
  ENDIF  
  **Ash 08/01/2001
  IF lnOldChrg < 0
    = gfModalGen('TRM42000B40011','DIALOG')
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

RETURN lcReturn

*--End of lfSoNvVal.
*!*************************************************************
*! Name      : LFLASTSCRN
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/06/2001
*! Purpose   : Call this function from Save Program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None
*!*************************************************************
*! Example   : =LFLASTSCRN()
*!*************************************************************
FUNCTION LFLASTSCRN
PRIVATE lcOrder , lcAlias , lnRecInvF , lnLastchrg

lcAlias = SELECT(0)
lnLastchrg = 0
llConsolJl = .T.
llGetInJl = .F.
SELECT (lcJLchargs)
lcOrder = ORDER()
SET ORDER TO TAG FULLINDEX

SELECT (lcInvFile)
lnRecInvF = RECNO(lcInvFile)

IF CONSOL = "Y"
  =SEEK(M.ACCOUNT)
  m.TotalChg = 0
  SCAN REST WHILE ACCOUNT + ORDER + STORE + PIKTKT = M.ACCOUNT FOR cSelect = '»' AND CONSOL = "N"
    IF !EMPTY(&lcInvFile..ORDER)
       llConsolJl = .F.

       m.TotalChg = m.TotalChg + &lcInvFile..ShipAmt + &lcInvFile..Freight + &lcInvFile..Insur +;
                                 &lcInvFile..Cod - ABS(&lcInvFile..ShipAmt * &lcInvFile..DiscPcnt/100)+;
                                 &lcInvFile..Tax_Amt + &lcInvFile..nPstAmt + &lcInvFile..nHstAmt

       =SEEK(&lcInvFile..ACCOUNT + &lcInvFile..ORDER + &lcInvFile..STORE + &lcInvFile..PIKTKT , lcJlChargs)
       lnLastchrg = lnLastchrg + &lcJlChargs..nchrg1 + &lcJlChargs..nchrg2 + &lcJlChargs..nchrg3 + &lcJlChargs..nchrg4 + &lcJlChargs..nchrg5 +;
                                 &lcJlChargs..nchrg6 + &lcJlChargs..nchrg7 + &lcJlChargs..nchrg8 + &lcJlChargs..nchrg9 + &lcJlChargs..nchrg10
    ENDIF
  ENDSCAN
ENDIF


IF llConsolJl
  m.TotalChg = &lcInvFile..ShipAmt + &lcInvFile..Freight + &lcInvFile..Insur +;
               &lcInvFile..Cod - ABS(&lcInvFile..ShipAmt * &lcInvFile..DiscPcnt/100)+;
               &lcInvFile..Tax_Amt + &lcInvFile..nPstAmt + &lcInvFile..nHstAmt

  SELECT (lcInvFile)
  =SEEK(&lcInvFile..ACCOUNT + &lcInvFile..ORDER + &lcInvFile..STORE + &lcInvFile..PIKTKT , lcJlChargs)
  lnLastchrg = &lcJlChargs..nchrg1 + &lcJlChargs..nchrg2 + &lcJlChargs..nchrg3 + &lcJlChargs..nchrg4 + &lcJlChargs..nchrg5 +;
               &lcJlChargs..nchrg6 + &lcJlChargs..nchrg7 + &lcJlChargs..nchrg8 + &lcJlChargs..nchrg9 + &lcJlChargs..nchrg10
ENDIF

m.TotalChg = m.TotalChg + lnLastchrg

SELECT (lcInvFile)
IF BETWEEN (lnRecInvF,1,RECCOUNT(lcInvFile))
  GOTO lnRecInvF IN (lcInvFile)
ENDIF

SELECT (lcJLchargs)
SET ORDER TO TAG &lcOrder

SELECT(lcAlias)

*--End of LFLASTSCRN.

*!**************************************************************************
*! Name      : lfSaveChrg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/24/2001
*! Purpose   : Update the temp file with the chargs.
*!**************************************************************************
*! Called from  : ARIINV.PRG (INVOICE SALES ORDER SCREEN)
*!**************************************************************************
*! Example     : = lfSaveChrg()
*!**************************************************************************
*
FUNCTION lfSaveChrg
PRIVATE lcFile , lcOldTag
lcFile = SELECT(0)

SELECT (lcJLchargs)
lcOldTag = TAG()
SET ORDER TO TAG FullIndex
=SEEK(&lcInvHdr..ACCOUNT + &lcInvHdr..ORDER + &lcInvHdr..STORE + &lcInvHdr..PIKTKT , lcJlChargs)
lnOldChrgs = nchrg1 + nchrg2 + nchrg3 + nchrg4 + nchrg5 + nchrg6 + nchrg7 + nchrg8 + nchrg9 + nchrg10

REPLACE nchrg1  WITH nModChrg1 * IIF(&lcJLchargs..nRatio1=1,1,&lcInvHdr..Ship),;
        nchrg2  WITH nModChrg2 * IIF(&lcJLchargs..nRatio2=1,1,&lcInvHdr..Ship),;
        nchrg3  WITH nModChrg3 * IIF(&lcJLchargs..nRatio3=1,1,&lcInvHdr..Ship),;
        nchrg4  WITH nModChrg4 * IIF(&lcJLchargs..nRatio4=1,1,&lcInvHdr..Ship),;
        nchrg5  WITH nModChrg5 * IIF(&lcJLchargs..nRatio5=1,1,&lcInvHdr..Ship),;
        nchrg6  WITH nModChrg6 * IIF(&lcJLchargs..nRatio6=1,1,&lcInvHdr..Ship),;
        nchrg7  WITH nModChrg7 * IIF(&lcJLchargs..nRatio7=1,1,&lcInvHdr..Ship),;
        nchrg8  WITH nModChrg8 * IIF(&lcJLchargs..nRatio8=1,1,&lcInvHdr..Ship),;
        nchrg9  WITH nModChrg9 * IIF(&lcJLchargs..nRatio9=1,1,&lcInvHdr..Ship),;
        nchrg10 WITH nModChrg10 * IIF(&lcJLchargs..nRatio10=1,1,&lcInvHdr..Ship)

*--Case the user modify the charges in the charges folder.
lnNewChrgs = nchrg1 + nchrg2 + nchrg3 + nchrg4 + nchrg5 + nchrg6 + nchrg7 + nchrg8 + nchrg9 + nchrg10
llChangJL = IIF(lnNewChrgs # lnOldChrgs , .T. , .F.)
IF llChangJL
  M.TOTALCHG = M.TOTALCHG + (lnNewChrgs - lnOldChrgs)
ENDIF

IF SEEK('M' +  &lcJLchargs..Account , 'CUSTOMER')
  lcAccount = ""
  lcAccount = &lcJLchargs..Account
  IF CUSTOMER.CONSOL = "Y"
    SELECT (lcJLchargs)
    lcKey = ORDER()
    SET ORDER TO TAG ORDER
    STORE 0 TO LnNCHRG1,LnNCHRG2,LnNCHRG3,LnNCHRG4,LnNCHRG5,LnNCHRG6,LnNCHRG7,LnNCHRG8,LnNCHRG9,LnNCHRG10

    IF SEEK(SPACE(6) + lcAccount , lcJLchargs)
      SCAN REST WHILE ORDER + ACCOUNT = ""  FOR !EMPTY(ORDER) AND &lcJLchargs..Account = lcAccount
        FOR lnNo = 1 TO 10
          lcNo = ALLTRIM(STR(lnNo))
          LnNCHRG&lcNo = LnNCHRG&lcNo + &lcJLchargs..NCHRG&lcNo
        ENDFOR
      ENDSCAN

      IF SEEK(SPACE(6) + lcAccount , lcJLchargs )
        REPLACE &lcJLchargs..NCHRG1   WITH LnNCHRG1  ,;
                &lcJLchargs..NCHRG2   WITH LnNCHRG2  ,;
                &lcJLchargs..NCHRG3   WITH LnNCHRG3  ,;
                &lcJLchargs..NCHRG4   WITH LnNCHRG4  ,;
                &lcJLchargs..NCHRG5   WITH LnNCHRG5  ,;
                &lcJLchargs..NCHRG6   WITH LnNCHRG6  ,;
                &lcJLchargs..NCHRG7   WITH LnNCHRG7  ,;
                &lcJLchargs..NCHRG8   WITH LnNCHRG8  ,;
                &lcJLchargs..NCHRG9   WITH LnNCHRG9  ,;
                &lcJLchargs..NCHRG10  WITH LnNCHRG10
      ENDIF
    ENDIF

    SET ORDER TO TAG CONSOLJL
    IF SEEK(lcAccount + SPACE(8) + "Y", lcJLchargs)
      SCAN REST WHILE ACCOUNT + STORE + CONSOL = ""  FOR  CONSOL = "N" AND &lcJLchargs..Account = lcAccount
        FOR lnNo = 1 TO 10
          lcNo = ALLTRIM(STR(lnNo))
          LnNCHRG&lcNo = LnNCHRG&lcNo + &lcJLchargs..NCHRG&lcNo
        ENDFOR
      ENDSCAN

      IF SEEK(lcAccount + SPACE(8) + "Y", lcJLchargs)
        REPLACE &lcJLchargs..NCHRG1   WITH LnNCHRG1  ,;
                &lcJLchargs..NCHRG2   WITH LnNCHRG2  ,;
                &lcJLchargs..NCHRG3   WITH LnNCHRG3  ,;
                &lcJLchargs..NCHRG4   WITH LnNCHRG4  ,;
                &lcJLchargs..NCHRG5   WITH LnNCHRG5  ,;
                &lcJLchargs..NCHRG6   WITH LnNCHRG6  ,;
                &lcJLchargs..NCHRG7   WITH LnNCHRG7  ,;
                &lcJLchargs..NCHRG8   WITH LnNCHRG8  ,;
                &lcJLchargs..NCHRG9   WITH LnNCHRG9  ,;
                &lcJLchargs..NCHRG10  WITH LnNCHRG10
      ENDIF
    ENDIF
    SELECT (lcJLchargs)
    SET ORDER TO TAG &lcKey
  ENDIF
ENDIF

SELECT (lcJlChargs)
SET ORDER TO TAG (lcOldTag)

SELECT(lcFile)
*--End of lfSaveChrg.


*!**************************************************************************
*! Name      : lfHoldRes
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/28/2002
*! Purpose   : Let the user select Hold reasons from a mover.
*!**************************************************************************
*! Example     : = lfHoldRes()
*!**************************************************************************
*!..C102556,1
FUNCTION lfHoldRes

*--Enable & Disable Reasons Button.
IF llEanabled
  SHOW GET pbReason ENABLE
ELSE
  SHOW GET pbReason DISABLE
ENDIF
*--If change status from Holsd to Open, Clear Hold reasons.
*B606157,1 ASH 02/03/2003 (Begin) Save the the exact setting.
lcSavExact = SET ('EXACT')
*B606157,1 ASH 02/03/2003 (End)
IF llClearReas
  SELECT ORDHDR
  =RLOCK()
  REPLACE DECL_CODE  WITH '',;
          DECL_CODE1 WITH '',;
          DECL_CODE2 WITH '',;
          DECL_CODE3 WITH ''
  UNLOCK
  RETURN
ENDIF

*--If Display Mover.
IF llPopMover
  *--Is it first time?
  llFirst = EMPTY(ORDHDR.DECL_CODE)  AND EMPTY(ORDHDR.DECL_CODE1) AND;
            EMPTY(ORDHDR.DECL_CODE2) AND EMPTY(ORDHDR.DECL_CODE3)
  DECLARE laRpTarget[1,2]
  IF llFirst
    STORE '' TO laRpTarget
  ELSE
    *--Fill Traget array with the previously saved values.
    IF !EMPTY(ORDHDR.DECL_CODE)
      laRpTarget[1,1] = ALLTRIM(ORDHDR.DECL_CODE)
    ENDIF
    IF !EMPTY(ORDHDR.DECL_CODE1)
      IF !EMPTY(laRpTarget)
        DECLARE laRpTarget[ALEN(laRpTarget,1)+1,1]
      ENDIF
      laRpTarget[ALEN(laRpTarget,1),1] = ALLTRIM(ORDHDR.DECL_CODE1)
    ENDIF
    IF !EMPTY(ORDHDR.DECL_CODE2)
      IF !EMPTY(laRpTarget)
        DECLARE laRpTarget[ALEN(laRpTarget,1)+1,1]
      ENDIF
      laRpTarget[ALEN(laRpTarget,1),1] = ALLTRIM(ORDHDR.DECL_CODE2)
    ENDIF
    IF !EMPTY(ORDHDR.DECL_CODE3)
      IF !EMPTY(laRpTarget)
        DECLARE laRpTarget[ALEN(laRpTarget,1)+1,1]
      ENDIF
      laRpTarget[ALEN(laRpTarget,1),1] = ALLTRIM(ORDHDR.DECL_CODE3)
    ENDIF
  ENDIF
            
  *--Fill arrays.
  DECLARE laRpSource[1,1]
  STORE '' TO laRpSource
  *--Get All Hold reasons.
  =gfwCodePop(@laCodes,'DECL_CODE','L')
  FOR lnFrom  = 1 TO ALEN(laReason,1)
    DECLARE laRpSource[lnFrom,1]
    *khalid
    *laRpSource[lnFrom,1] = laReason[lnFrom,1]
    laRpSource[lnFrom,1] = ALLTRIM(laReason[lnFrom,1])
    *khalid
  ENDFOR
  =ACOPY(laReason,laUpdate)
  *B606157,1 ASH 02/03/2003 (Begin) Sometime the codes begin with spaces.
  lnLen = ALEN(laUpdate,1)
  FOR I = 1 TO lnLen 
    laUpdate[I] = ALLTRIM(laUpdate[I])
  ENDFOR
  *B606157,1 ASH 02/03/2003 (End) 
 
  IF llFirst
    *--Get Default.
    *B606157,1 ASH 02/03/2003 (Begin) Don't get the default reason.
    *=gfwCodePop(@laCodes,'DECL_CODE','D')
    *laRpTarget[1] = laReason[lnReason,1]
    *B606157,1 ASH 02/03/2003 (End)
  ELSE
    *--Replace Target array's codes with its Description names.
    FOR lnFrom  = 1 TO ALEN(laRpTarget,1)
      LnElement = ASCAN(laUpdate,ALLTRIM(laRpTarget[lnFrom,1]))
      LnElement = ASUBSCRIPT(laUpdate,LnElement,1)
      IF LnElement <> 0
        *khalid
        *laRpTarget[lnFrom,1] = laReason[LnElement,1]
        laRpTarget[lnFrom,1] = ALLTRIM(laReason[LnElement,1])
        *khalid
      ENDIF
    ENDFOR
  ENDIF  
  DO WHILE .T.
    *B606157,1 ASH 02/03/2003 (Begin) Set exact on to get the exact descritpion.
    SET EXACT ON
    *B606157,1 ASH 02/03/2003 (End)
    
    = gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.  
    *--If the user didn't choose any reason, Reset it to Open.
    *B606157,1 ASH 02/03/2003 (Begin) Let the user leave the hold reason empty.
    *IF EMPTY(laRpTarget)
    *   lnStatus = 1
    *   SHOW GET lnStatus
    *   SHOW GET pbReason DISABLE       
    *   REPLACE ORDHDR.STATUS WITH 'O'
    *    SHOW WINDOW (lcOrdTitl) REFRESH SAME       
    *   RETURN .F.
    *ENDIF
    *B606157,1 ASH 02/03/2003 (End)
    IF ALEN(laRpTarget,1) >4
       = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Hold reasons cannot be more than 4 reasons!')             
       LOOP
    ENDIF
    *--Update reasons
    SELECT ORDHDR
    =RLOCK()
    REPLACE DECL_CODE  WITH '',;
            DECL_CODE1 WITH '',;
            DECL_CODE2 WITH '',;
            DECL_CODE3 WITH ''
               
    FOR lnFrom = 1 TO ALEN(laRpTarget,1)
      IF EMPTY(ALLTRIM(laRpTarget[lnFrom]))
        LOOP
      ENDIF
      *--B606335,1 RAE
      *LnElement = ASUBSCRIPT(laUpdate,ASCAN(laUpdate,laRpTarget[lnFrom]),1)
      *B606157,1 ASH 02/03/2003 (Begin) Use the first element of laRpTarget array.
      *LnElement = ASCAN(laUpdate,ALLTRIM(laRpTarget[lnFrom,1]))
      LnElement = ASCAN(laUpdate,ALLTRIM(laRpTarget[lnFrom]))
      *B606157,1 ASH 02/03/2003 (End)
      LnElement = ASUBSCRIPT(laUpdate,LnElement,1)
      *--B606335,1 RAE
      IF lnFrom = 1
        REPLACE DECL_CODE WITH SUBSTR(laUpdate[LnElement,2],1,6)
      ELSE
        lcFrom  = STR(lnFrom-1,1) 
        REPLACE DECL_CODE&lcFrom WITH SUBSTR(laUpdate[LnElement,2],1,6)
      ENDIF  
    ENDFOR
    *B606157,1 ASH 02/03/2003 (Begin) Restore the old exact setting.
    SET EXACT &lcSavExact
    *B606157,1 ASH 02/03/2003 (End)
    UNLOCK
    SHOW WINDOW (lcOrdTitl) REFRESH SAME
    EXIT
  ENDDO
ENDIF
*B606157,1 ASH 02/03/2003 (Begin) Restore the old exact setting.
SET EXACT &lcSavExact
*B606157,1 ASH 02/03/2003 (End)
