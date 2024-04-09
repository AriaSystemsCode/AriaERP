*!***************************************************************************
*! Program file  : Al2CSV.PRG
*! Program desc. : Export PIKTKT Information to CSV file for TOWER HOUSE
*! Date          : 04/10/2005
*! System        : Aria Advantage Series.
*! Module        : SALES ORDERS ALOCATION (AL)
*! Developer     : MOHAMED SHOKRY (MHM)
*! Tracking Job Number: C126442
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Notes   : ....
*!***************************************************************************
*! Example : DO AL2CSV
*!***************************************************************************
*! Modifications :
*!***************************************************************************
PRIVATE lcExpr , lcTmpFile , lcOutFile,lnOutFile , lnClrPos, lnClrLn
STORE '' TO lcExpr , lcTmpFile , lcOutFile
STORE 0 TO lnOutFile ,lnClrPoS ,lnClrLn

IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH') 
ENDIF
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH') 
ENDIF
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH') 
ENDIF
IF !USED('STYLE')
  = gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
ENDIF
IF !USED('PIKTKT')
  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'PIKTKT','SH') 
ENDIF
IF !USED('SCALE')
  = gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH') 
ENDIF

lcEscapeKy = ON("KEY","ESC")
lcExpr     = gfOpGrid('AL2CSV' , .T.)

IF EMPTY(LCEXPR)
  =FFLUSH(lcOutFile)
  =FCLOSE(lnOutFile)
  RETURN
ELSE
  IF !('PIKTKT' $ (lcExpr))
     lcExpr = lcExpr + " .AND. PIKTKT.STATUS = 'O' .AND. !PIKTKT.lexprtocsv "
  ENDIF  
ENDIF
IF EMPTY(lcOutFile)
  =gfModalGen("INM00000B00000","DIALOG",'','','There is no Output file selected')
  RETURN
ENDIF
ON KEY LABEL ESCAPE &lcEscapeKy
lcTmpFile  = gfTempName()

=lfCreatTmp()
=lfColData()
IF RECCOUNT(lcTmpFile) > 0
  =lfCsvRec()
ELSE
  =gfModalGen("INM00000B00000","DIALOG",'','','There are no records matched')
  =FFLUSH(lcOutFile)
  =FCLOSE(lnOutFile)
  RETURN
ENDIF
=FFLUSH(lcOutFile)
=FCLOSE(lnOutFile)

*-Message : 000000
*-Output file has been created
*-Button : 00000
*-Ok
=gfModalGen('TRM00000B00000','ALERT','','','Ascii .CSV file '+lcOutFile+' has been created.')

IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
IF USED('STYLE')
  USE IN STYLE
ENDIF
IF USED('SCALE')
  USE IN SCALE
ENDIF
IF USED('PIKTKT')
  USE IN PIKTKT
ENDIF

*!***************************************************************************
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Create the Temp files that will use to print the Text File
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCreatTmp()
*!***************************************************************************
FUNCTION lfCreatTmp
CREATE TABLE (gcWorkDir+lcTmpFile) (PIKTKT C(6),Account C(5) , DATE D(8), ORDER C(6),Address1 C(30)  ,;
              Address2 C(30) , Address3 C(30) ,Address4 C(30) ,Address5 C(30) ,Address6 C(30), Address7 C(30) ,;
              CustPo C(15),Style C(19),Desc C(30) , ClrDesc C(20) , Size C(6) , Qty N(10))
INDEX ON (PIKTKT + ORDER) TAG (lcTmpFile)  OF (lcTmpFile)

*--End of Function lfCreatTmp.
*!*************************************************************
*! Name      : lfColData
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Collecting Data.
*!*************************************************************
*! Calls     : AL2CSV.PRG
*!*************************************************************
*! Example   :  =lfColData()
*!*************************************************************
FUNCTION lfColData
*-- Check if the output file allready exists.
IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file

lnOutFile = FCREATE(lcOutFile)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
=lfWriteOrd()

*--End of Function lfColData.
*!*************************************************************
*! Name      : lfWriteOrd
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Write an output Order text file with ext. CSV
*!*************************************************************
*! Called from : AL2CSV.PRG
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWriteOrd
PRIVATE lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,;
        lcAddress6 , lcAddress7 , lcShpTName
STORE '' TO lcAddress1,lcAddress2,lcAddress3,lcAddress4,lcAddress5,;
            lcAddress6 , lcAddress7 , lcShpTName
WAIT 'Creating outbound PIKTKT file...' WINDOW NOWAIT
*-- First write Invoice records

=lfChkStrct()
SELECT CUSTOMER
SET RELATION TO


SELECT PIKTKT
SET RELATION TO
SET RELATION TO 'M' + PIKTKT.ACCOUNT INTO CUSTOMER ADDITIVE

SCAN FOR &lcExpr 
  *--mhm204
  IF lexprtocsv
    LOOP
  ENDIF
  =SEEK('O'+ PIKTKT.ORDER,'ORDHDR')
  =lfGetAdd()
  *--mhm204
  
  IF SEEK('O'+ PIKTKT.ORDER,'ORDLINE')
    REPLACE lexprtocsv WITH .T.
    SELECT ORDLINE
    SCAN REST WHILE cOrdType + Order = 'O'+ PIKTKT.ORDER
      IF  PIKTKT.PIKTKT <> OrdLine.PIKTKT
        LOOP
      ENDIF
      =SEEK('O'+ PIKTKT.ORDER,'ORDHDR')
      =SEEK(ORDLINE.Style,'STYLE')
      =SEEK('S'+OrdLine.Scale,'SCALE')
      lcColorDec = gfCodDes(SUBSTR(Style,lnClrPos ,lnClrLn),'COLOR')

      FOR lnCount = 1 TO Scale.Cnt
        lcCount = STR(lnCount,1)
        *--mhm
        IF EMPTY(OrdLine.PIK&lcCount)
          LOOP
        ENDIF
        *--mhm
        SELECT (lcTmpFile)
        APPEND BLANK
        REPLACE  Order    WITH PIKTKT.ORDER               ,;          	 
                 PIKTKT   WITH PIKTKT.PIKTKT              ,;
                 Account  WITH PIKTKT.Account             ,;
                 Date     WITH PIKTKT.DATE                ,;
                 Address1 WITH lcShpTName                 ,;
                 Address2 WITH lcAddress1                 ,;
                 Address3 WITH lcAddress2                 ,;
                 Address4 WITH lcAddress3                 ,;
                 Address5 WITH lcAddress4                 ,;
                 Address6 WITH lcAddress5                 ,;
                 Address7 WITH lcAddress6                 ,;
                 CustPo   WITH ORDHDR.CustPo              ,;
                 Style    WITH ORDLINE.Style              ,;
                 Desc     WITH Style.Desc                 ,;
                 ClrDesc  WITH lcColorDec                 ,;
                 Size     WITH SCALE.Sz&lcCount           ,;
                 Qty      WITH OrdLine.PIK&lcCount  
      ENDFOR             
    ENDSCAN        
  ENDIF    
ENDSCAN
*--End of Function lfWriteOrd.
*!*************************************************************
*! Name      : lfCsvRec
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Write O records in the output orders text file
*!*************************************************************
*! Called from : AL2CSV
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfCsvRec
PRIVATE lcSegLine 

SELECT (lcTmpFile)
SCAN

  lcSegLine = ''

  *-- Piktkt No.
  lcSegLine = lcSegLine + LEFT(ALLTRIM(PIKTKT),6)+','

  *-- Customer Code (Ship To customer Code)
  lcSegLine = lcSegLine + LEFT(Account,6) + ','

  *-- Date
  lcSegLine = lcSegLine + DTOC(Date) +','

  *-- Order Number
  lcSegLine = lcSegLine + LEFT(ALLTRIM(ORDER),6)+','

  *-- Customer Address12 (address 1)
  lcSegLine = lcSegLine + LEFT(Address1,30) + ','

  *-- Customer Address22 (address 2)
  lcSegLine = lcSegLine + LEFT(Address2,30) + ','

  *-- Customer Address33 (address 3)
  lcSegLine = lcSegLine + LEFT(Address3,30) + ','

  *-- Customer Address44 (address 4)
  lcSegLine = lcSegLine + LEFT(Address4,30) + ','

  *-- Customer Address55 (address 5)
  lcSegLine = lcSegLine + LEFT(Address5,30) + ','

  *-- Customer Address66 (address 6)
  lcSegLine = lcSegLine +LEFT(Address6,30) + ','

  *-- Customer Address77 (address 7)
  lcSegLine = lcSegLine + LEFT(Address7,30) + ','

  *-- Customer CustPo   (CustPo)
  lcSegLine = lcSegLine + LEFT(CustPo,15) + ','
  
  *-- Style   (Style)
  lcSegLine = lcSegLine + LEFT(Style,19) + ','

  *-- Style Desc  
  lcSegLine = lcSegLine + LEFT(Desc,30) + ','

  *-- color Desc  
  lcSegLine = lcSegLine + LEFT(ClrDesc,20)   + ','

  *-- size Desc  
  lcSegLine = lcSegLine + LEFT(Size,6)   + ','

  *-- Qty
  lcSegLine = lcSegLine + STR(Qty,10) + ','


  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
*--End of Function lfCsvRec.
*!*************************************************************
*! Name      : lfGetFile
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Ref       :  
*!*************************************************************
*! Synopsis : Give ability to user to choose name and path of
*!            the output file.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfGetFile
PRIVATE ltOutFile , lcTalk
lcOutFile = ""

ltOutFile = PUTFILE('Select the output file' , lcOutFile , "CSV" )

IF !EMPTY(ltOutFile)
  lcOutFile = ltOutFile 
  lcTalk = SET('TALK')
  SET TALK OFF
  SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
  SET TALK &lcTalk.
ENDIF  

*!*************************************************************
*! Name      : lfsrPkt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrPkt()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
FUNCTION lfsrPkt
PARAMETERS lcParm

DO CASE

  CASE  lcParm = 'S'
    SELECT PIKTKT
    GO TOP
    lcTmpCust = ALLTRIM(laOgFxFlt[3,6])
    ldLftDat = LEFT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)
    ldRghDat = RIGHT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)
    DO CASE
      CASE USED(lcTmpCust) AND EMPTY(ldLftDat)
          SELECT (lcTmpCust)
          LOCATE
          IF !EOF()
            SELECT PIKTKT
            SET RELATION TO  PIKTKT.ACCOUNT INTO &lcTmpCust ADDITIVE
            SET FILTER TO Account = &lcTmpCust..account 
          ENDIF 
  
      CASE !USED(lcTmpCust) AND !EMPTY(ldLftDat)
            SELECT PIKTKT
            SET FILTER TO BETWEEN(PIKTKT.date,CTOD(LEFT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)),CTOD(RIGHT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)))
      
      CASE USED(lcTmpCust) AND !EMPTY(ldLftDat)
          SELECT (lcTmpCust)
          LOCATE
          IF !EOF()
            SELECT PIKTKT
            SET RELATION TO  PIKTKT.ACCOUNT INTO &lcTmpCust ADDITIVE
            SET FILTER TO Account = &lcTmpCust..account AND;
            BETWEEN(PIKTKT.date,CTOD(LEFT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)),CTOD(RIGHT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)))
          ELSE
            SELECT PIKTKT
            SET FILTER TO BETWEEN(PIKTKT.date,CTOD(LEFT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)),CTOD(RIGHT(laOgFxFlt[2,6],AT('|',laOgFxFlt[2,6])-1)))
          ENDIF 
      
    ENDCASE
    
  CASE  lcParm = 'R'
        SELECT PIKTKT
        SET FILTER TO 
        SET RELATION TO

ENDCASE  
*-- End of lfsrPkt.

*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Get the Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLn  = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR



*!*************************************************************
*! Name      : lfGetAdd
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/10/2005
*! Purpose   : Get the Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfGetAdd()
*!*************************************************************
FUNCTION lfGetAdd

IF ORDHDR.Alt_ShpTo

  lcShipName  = ORDHDR.STName  
  lcAddress1 = ORDHDR.cAddress1
  lcAddress2 = ORDHDR.cAddress2
  lcAddress3 = ORDHDR.cAddress3
  lcAddress4 = ORDHDR.cAddress4
  lcAddress5 = ORDHDR.cAddress5
ELSE
    PRIVATE lcAlias , lnCUSRec
    lnCUSRec = 0
    lcAlias = ALIAS()
    *B122346,1 BWA 04/01/2004.[END]
    
    IF !EMPTY(Piktkt.Store)
      =SEEK('S'+Piktkt.Account+Piktkt.Store,'CUSTOMER') 

      lcAlias = ALIAS()
      SELECT CUSTOMER
      IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
        lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
        =SEEK('S'+Piktkt.Account+CUSTOMER.Dist_ctr)
      ENDIF
    ENDIF
   
    lcShipName  =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')

    IF lnCUSRec <> 0 
      GOTO lnCUSRec IN CUSTOMER
	ENDIF
    SELECT(lcAlias)

    lcShpTName = IIF(EMPTY(CUSTOMER.DBA) , CUSTOMER.STName , CUSTOMER.DBA)
    lcAddress1 = CUSTOMER.cAddress1
    lcAddress2 = CUSTOMER.cAddress2
    lcAddress3 = CUSTOMER.cAddress3
    lcAddress4 = CUSTOMER.cAddress4
    lcAddress5 = CUSTOMER.cAddress5
    lcAddress6 = CUSTOMER.cAddress6

    lcAddress1 = STRTRAN(lcAddress1 , ',' , ' ' )
    lcAddress2 = STRTRAN(lcAddress2 , ',' , ' ' )
    lcAddress3 = STRTRAN(lcAddress3 , ',' , ' ' )
    lcAddress4 = STRTRAN(lcAddress4 , ',' , ' ' )
    lcAddress5 = STRTRAN(lcAddress5 , ',' , ' ' )
    lcAddress6 = STRTRAN(lcAddress6 , ',' , ' ' )

ENDIF
