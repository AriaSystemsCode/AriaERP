*:*********************************************************
*: Program file  : FLEXPACK.prg
*: Program desc. : MBI Excel Packing Slip - FRANK LYMAN
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 06/25/2009
***********************************************************
CLOSE ALL 
lnHndl = FCREATE('RUN2.TXT')
IF lnHndl<0
   MESSAGEBOX('The report is already running','Aria systems')
   RETURN 
ENDIF 


SET SAFETY OFF  
_screen.WindowState = 2                 && Maximized window
_screen.Caption = 'Packing Slip'

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
IF TYPE('laFls')='U'
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
      MESSAGEBOX('One file or more is wrong','Aria Systems')
      RETURN 
  ENDCASE   
  GO TOP
  DELETE 
  PACK 
  GO TOP 
ENDFOR

IF llErr = .T.
  MESSAGEBOX('Cannot Resume','Aria Systems')
  RETURN 
ENDIF

SAVE TO (lcMemFile ) ALL LIKE lcDir

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
=lfupdmultscl()
*CSC,1     TMI 11/11/2009 02:49:47 PM [End  ] Create a new file based on the lcTempScale one that holds scales in memo fields

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
SET RELATION TO ALLTRIM(A) INTO (lcTempHeader) ADDITIVE 
SET RELATION TO ALLTRIM(A) INTO (lcTempScale) ADDITIVE 
LOCATE


*tmi [start] 09/21/2009 Get the ST line #, delete the type T befor it 
LOCATE FOR D = 'ST'
lnST = RECNO()
SET DELETED ON 
llKeep = .T.
*tmi [ end ] 09/21/2009 Get the ST line #, delete the type T befor it 
 
SCAN  
  *tmi , Remove R,C
  *IF ALLTRIM(D) $ 'MRC'
  IF PADR(ALLTRIM(D),1) $ 'RC'
    DELETE     
    *tmi
    *SET DELETED ON 
    *tmi
  ENDIF         
  *TMI empty lines with type T  located before ST
  IF PADR(ALLTRIM(D),1) = 'T'
    IF RECNO() <= lnST 
      REPLACE V WITH ''
    ELSE
      DELETE
    ENDIF
  ENDIF
  IF PADR(ALLTRIM(D),1) = 'M'
    IF RECNO() < lnST
      IF llKeep  && skip lines with type M just after ST
        SCAN REST WHILE PADR(ALLTRIM(D),1) = 'M'
        ENDSCAN
        SKIP -1 && the outer scan will do this skip
        llKeep = .F.        
      ENDIF      
    ELSE
      DELETE
    ENDIF
  ENDIF
  
  
ENDSCAN
 
SCAN  
  IF D = 'ST'
    DELETE 
    SET DELETED ON 
  ENDIF       
ENDSCAN

*AHS 23/09/2009 To Remove the blank lines in the packing slip [Start]
SELECT (lcTempDetail) 
SCAN 
  IF &lcTempDetail..D ='T'
    DELETE 
  ENDIF   
ENDSCAN 
*AHS [End]


*          TMI 10/23/2009 07:21:10 PM [Start] Get page numbers
lnPageLines = 26 && No of lines in page
SELECT (lcTempDetail)
LOCATE
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


SET DEVICE TO SCREEN 
*          TMI 10/23/2009 05:25:37 PM [Start] 
*DO CASE
*  CASE &lcTempHeader..B ='E'
*    REPORT FORM PackingE.frx TO PRINTER PROMPT NODIALOG PREVIEW  
*  CASE &lcTempHeader..B ='F'   
*          TMI 10/23/2009 05:25:46 PM [Start] 
    REPORT FORM Packing.frx TO PRINTER PROMPT NODIALOG PREVIEW
*          TMI 10/23/2009 05:25:48 PM [Start] 
*ENDCASE
*          TMI 10/23/2009 05:25:51 PM [End  ] 



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

=FCLOSE(lnHndl)
ERASE RUN2.TXT

*ERASE *.dbf
*ERASE *.cdx
*ERASE *.bak 

RETURN 





 
 


