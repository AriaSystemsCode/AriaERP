***************************************************************
*: Program file  : ConvUser.PRG
*: Program desc. : Convert the user code from Aims code file to 
*:                 A27 code file as category code.
*: Module        : 
*:         System: Aria Apparel System
*:      Developer: Ashraf Sherif Mohammad (ASH)
*:*************************************************************
*: Calls 		 : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : 
*:*************************************************************
*: Date : 09/12/99
*:*************************************************************
*: C101653,1 
*:*************************************************************

CLOSE DATA
SET CONFIRM OFF
SET SAFETY OFF
PUBLIC XAMS_DATA,XARA_DATA

XAMS_DATA = "F:\AIMS107\DATA1\             "
XARA_DATA = "F:\ARIA27\DBFS\               "

DO WHILE .T.
  CLEAR
  @ 10,10 SAY 'AIMS DATA DIRECTORY ' GET XAMS_DATA PICTURE '@!'
  @ 11,10 SAY 'ARIA DATA DIRECTORY ' GET XARA_DATA PICTURE '@!'
  READ
  CHOICE = 'C'
  @ 23,00
  @ 23,20 SAY '<P>roceed <M>odify <C>ancel ? '  GET CHOICE PICTURE '!' ;
  VALID CHOICE $ 'PMC'
  READ
  DO CASE
    CASE CHOICE = 'P'
      EXIT
    CASE CHOICE = 'M'
      LOOP
    OTHERWISE
      CANCEL
  ENDCASE
ENDDO

XAMS_DATA = ALLTRIM(XAMS_DATA)
XARA_DATA = ALLTRIM(XARA_DATA)

SELE 0
USE (XAMS_DATA+'CODE') ALIAS OLD
SET ORDER TO 1

SELE 0
USE (XARA_DATA+'CODES') ALIAS NEW

SELE 0
USE (XARA_DATA+'OrdHdr') 
SET ORDER TO TAG ORDHDR 

SELE 0
USE (XAMS_DATA+'ORDERHDR') 


WAIT WINDOW 'Converting Users Code.' NOWAIT

SELECT OLD
SEEK('U')
llFirst = .T.
SCAN WHILE CODE= 'U'
  WAIT WINDOW CODE NOWAIT
  SELECT NEW
  APPEND BLANK
  REPLACE cDefCode  WITH IIF(llFirst,'D','N') ,;
          cFld_Name WITH 'CORDERCAT' ,;
          cDiscRep  WITH Old.cData   ,;  
          CRLTFIELD WITH 'N'         ,;
          cCode_No  WITH SUBSTR(Old.Code,2,2)
  IF llFirst
    APPEND BLANK
    REPLACE cDefCode  WITH 'N'         ,;
            cFld_Name WITH 'CORDERCAT' ,;
            cDiscRep  WITH Old.cData   ,;  
            CRLTFIELD WITH 'N'         ,;
            cCode_No  WITH SUBSTR(Old.Code,2,2)
  
    llFirst = .F.          
  ENDIF
ENDSCAN
SELECT ORDERHDR
SCAN FOR !EMPTY(USERCODE)
  WAIT WINDOW ORDER NOWAIT
  IF SEEK('O'+Order,'OrdHdr')
    REPLACE OrdHdr.cOrderCat WITH UserCode
  ENDIF
ENDSCAN
WAIT CLEAR