*:*********************************************************
*: Program file  : INVOICE.prg
*: Program desc. : MBI Excel Invoice - FRANK LYMAN
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 06/24/2009
***********************************************************
CLOSE ALL 
lnHndl = FCREATE('RUN.TXT')
IF lnHndl<0
   MESSAGEBOX('The report is already running','Aria systems')
   RETURN 
ENDIF    
SET SAFETY OFF  
_screen.WindowState = 2                 && Maximized window
_screen.Caption = 'Invoice'

lcPath =  SYS(5) + SYS(2003)+'\'     

lcCurrPath = ADDBS(justpath(SYS(16)))
lcMemFile = lcCurrPath+JUSTSTEM(PROGRAM())+'.MEM'
IF FILE(lcMemFile)
  RESTORE FROM (lcMemFile ) ADDITIVE 
ELSE 

  lcDir = GETDIR()
  IF EMPTY(lcDir)
    MESSAGEBOX('No folder selected','Aria systems')
    RETURN 
  endif  
  *SAVE TO (lcMemFile ) ALL LIKE lcDir
ENDIF 

llErr = .F.
ON ERROR llErr = .T.
=ADIR(laFls,lcDir+'*.*')
IF TYPE('laFls') = 'U'
  MESSAGEBOX('Cannot Continue, can not locate the folder '+lcDir,'Aria Systems')
  ERASE (lcMemFile)
  RETURN 
ENDIF


IF ALEN(laFls,1)<>3
  MESSAGEBOX('You must choose the 3 valid files','Aria Systems')
  RETURN 
ENDIF


FOR lni = 1 TO ALEN(laFls,1)
  SELECT 0  
  IMPORT from (lcDir+laFls[lnI,1]) TYPE xls
  DO case
    CASE C = 'SIZE_CODE'
      lcTempScale = ALIAS()
    CASE C = 'RESERVED1'
      lcTempDetail = ALIAS()
    CASE C = 'CO_NAME'
      lcTempHeader = ALIAS()
    OTHERWISE 
      MESSAGEBOX('Wrong file','Aria Systems')
      RETURN 
  ENDCASE   
  GO TOP
  DELETE 
  PACK 
  GO TOP 
ENDFOR

IF llErr = .T.
  MESSAGEBOX('Cannot Continue','Aria Systems')
  RETURN 
ENDIF

SAVE TO (lcMemFile ) ALL LIKE lcDir

ON ERROR 



*!*	lcDetail = GETFILE("xls")               && Getting Detail Excel Sheet
*!*	IF !FILE(lcDetail)
*!*	  MESSAGEBOX('Wrong File','Aria Systems')
*!*	  RETURN 
*!*	ENDIF 

*!*	SELECT 0
*!*	IMPORT from (lcDetail) TYPE xls
*!*	lcTempDetail = ALIAS()
*!*	GO TOP 
*!*	DELETE 
*!*	PACK

*!*	lcHeader = GETFILE("xls")               && Getting Header Excel Sheet
*!*	IF !FILE(lcHeader)
*!*	  MESSAGEBOX('Wrong File','Aria Systems')
*!*	  RETURN 
*!*	ENDIF 

*!*	SELECT 0
*!*	IMPORT from (lcHeader) TYPE xls
*!*	lcTempHeader = ALIAS()
*!*	GO TOP 
*!*	DELETE 
*!*	PACK


*!*	lcScale = GETFILE("xls")               && Getting Scale Excel Sheet
*!*	IF !FILE(lcScale)
*!*	  MESSAGEBOX('Wrong File','Aria Systems')
*!*	  RETURN 
*!*	ENDIF 

*!*	SELECT 0
*!*	IMPORT from (lcScale) TYPE xls
*!*	lcTempScale = ALIAS()
*!*	GO TOP 
*!*	DELETE 
*!*	PACK


*          TMI 10/23/2009 07:32:42 PM [Start] 
ALTER TABLE (lcTempHeader) add column PAGECOUNT N(3)
*          TMI 10/23/2009 07:32:45 PM [End  ]    

SELECT (lcTempDetail)
  INDEX on A+B TAG Invline
SELECT (lcTempHeader)
  INDEX on A TAG Invhdr 
SELECT (lcTempScale)
 INDEX on A TAG Invhdr 

*CSC,1     TMI 11/11/2009 02:49:47 PM [Start] Create a new file based on the lcTempScale one that holds scales in memo fields
lfUpdmultScl()
*CSC,1     TMI 11/11/2009 02:49:48 PM [End  ]   


*TMI 10/14/2009 [start] Update the currency symbol in the header temp file
SELECT (lcTempHeader)
LOCATE
REPLACE AS WITH IIF('E'$AS,CHRTRAN(AS,'E','€'),IIF('G'$AS,CHRTRAN(AS,'G','£'),AS)) ;
        AQ WITH IIF('E'$AQ,CHRTRAN(AQ,'E','€'),IIF('G'$AQ,CHRTRAN(AQ,'G','£'),AQ)) ;
        ALL
LOCATE
*TMI 10/14/2009 [ end ] Update the currency symbol in the header temp file

SELECT (lcTempDetail)
*TMI update the detail according the last call with Jack [start] 10/15/2009
LOCATE
SCAN FOR ALLTRIM(D) == 'S'
  REPLACE S WITH ALLTRIM(T)
  REPLACE T WITH ALLTRIM(U)
  REPLACE U WITH ALLTRIM(V)
  REPLACE V WITH ''
ENDSCAN
LOCATE
*TMI

*          TMI 10/23/2009 07:21:10 PM [Start] Get page numbers
lnPageLines = 33 && No of lines in page
SELECT (lcTempDetail)
DO WHILE !EOF()
  lcInv = A
  lnLines = 0
  COUNT TO lnLines REST WHILE A = lcInv
  =SEEK(lcInv,lcTempHeader)
  SELECT (lcTempHeader)
  REPLACE PAGECOUNT WITH CEILING(lnLines/lnPageLines)
  SELECT (lcTempDetail)
ENDDO
*          TMI 10/23/2009 07:21:10 PM [End  ] 

SET RELATION TO ALLTRIM(A) INTO (lcTempHeader) ADDITIVE 
SET RELATION TO ALLTRIM(A) INTO (lcTempScale) ADDITIVE 
LOCATE

*TMI 10/14/2009 [start] no need for these fields
*ALTER TABLE (lcTempDetail) add column FinTotal c(8)
*ALTER TABLE (lcTempDetail) add column Currency c(8)
*TMI 10/14/2009 [ end ] no need for these fields


*!*	SCAN 
*!*	  IF V ='SUBTOTAL'
*!*	     lcTot1 = U
*!*	  ENDIF 
*!*	  IF 'TPS' $ V
*!*	     lcTot2 = U
*!*	  ENDIF 
*!*	  IF 'QUEBEC' $ V
*!*	     lcTot3 = U
*!*	  ENDIF 
*!*	  lcFinTot = VAL(lcTot1)+VAL(lcTot2)+VAL(lcTot3)
*!*	ENDSCAN
*!*	STORE '' TO lcCurr
*!*	SELECT (lcTempHeader) 
*!*	SCAN 
*!*	   DO CASE 
*!*	      CASE '£' $ AQ
*!*	           lcCurr = '£'
*!*	           *replace currency WITH '£' ALL 
*!*	      CASE '$' $ AQ
*!*	           lcCurr = '$'
*!*	           *replace currency WITH '$' ALL 
*!*	      CASE '€' $ AQ
*!*	           lcCurr = '€'
*!*	           *replace currency WITH '€' ALL      
*!*	   ENDCASE 
*!*	ENDSCAN 

*!*	SELECT (lcTempDetail) 
*!*	       replace currency WITH lcCurr ALL  
*AHS
*!*	SELECT (lcTempHeader)
*!*	STORE '' TO lcInv  
*!*	SCAN
*!*	   lcInv = SUBSTR(A,4,7)
*!*	   replace A WITH lcInv
*!*	ENDSCAN 
*AHS

SET DEVICE TO SCREEN 


*          TMI 10/23/2009 05:24:56 PM [Start] comment out this code
*DO CASE
*  CASE &lcTempHeader..B ='E'
*    REPORT FORM InvoiceE.frx TO PRINTER PROMPT NODIALOG PREVIEW  
*  CASE &lcTempHeader..B ='F'   
*          TMI 10/23/2009 05:25:06 PM [End  ] 

REPORT FORM Invoice.frx TO PRINTER PROMPT NODIALOG PREVIEW

*          TMI 10/23/2009 05:25:08 PM [Start] 
*ENDCASE
*          TMI 10/23/2009 05:25:10 PM [End  ] 

try
SELECT (lcTempDetail) 
lcFile = SUBSTR(DBF(),1,LEN(DBF())-4)+'.*'
USE 
ERASE (lcFile)

SELECT (lcTempHeader)
lcFile = SUBSTR(DBF(),1,LEN(DBF())-4)+'.*'
USE 
ERASE (lcFile)

SELECT (lcTempScale)
lcFile = SUBSTR(DBF(),1,LEN(DBF())-4)+'.*'
USE 
ERASE (lcFile)
CATCH
ENDTRY

=FCLOSE(lnHndl)
ERASE RUN.TXT
*!*	ERASE *.dbf
*!*	ERASE *.cdx
*!*	ERASE *.bak     

RETURN 











 
 


*:**************************************************************************
*:* Name        : lfTotal
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/25/2009
*:* Purpose     : Get the total expression to be printed on the invoice
*:***************************************************************************
*:* Called from : FRX
*:***************************************************************************
FUNCTION lfTotal
PARAMETERS lcFld
LOCAL lcRet,lcVal 
lcRet = 'CONTINUE'
IF _pageno=EVAL(lcTempHeader+'.PAGECOUNT')

  lcVal = ALLTRIM(EVAL(lcTempHeader+'.'+lcFld))
  lcRet = LEFT(lcVal,1)+;
          ALLTRIM(TRANSFORM(VAL(SUBSTR(lcVal,2)),'99,999,999.99'))
  
ENDIF
RETURN lcRet
*-- end of lfTotal.


