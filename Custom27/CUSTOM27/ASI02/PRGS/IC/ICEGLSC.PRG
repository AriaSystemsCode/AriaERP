*****************************************************************************
*: Program file  : ICEGL01.PRG  (C# 101933)
*: Program desc. : Importing Style Master from Excel file For Customer Eagle's Eye.
*: Module        : IC
*:        System : Aria Apparel System (A2.7)
*:     Developer : AHMED MAHER - (AMH)
*****************************************************************************
*: Notes         : This program use Excel file saved as (*.CSV) to importing 
*:               : Style Master instead of enter manualy.
*****************************************************************************
*: Passed Parameters  : None.
*****************************************************************************

*-- Initializing the necessary variables.
DIMENSION laStr[13,4]  && Array of the Structure of the *.CSV File
laStr[1,1]='Div'
laStr[2,1]='Se/Yr'
laStr[3,1]='Style'
laStr[4,1]='Color'
laStr[5,1]='A5'
laStr[6,1]='Size'
laStr[7,1]='Desc'
laStr[8,1]='Desc2'
laStr[9,1]='Group'
laStr[10,1]='Pur_Grp'
laStr[11,1]='Sell_Price'
laStr[12,1]='MSRP'
laStr[13,1]='Retail'

FOR lnCnt = 1 TO 13
  laStr[lnCnt,2]='C'
ENDFOR

laStr[1,3]=25
laStr[2,3]=16
laStr[3,3]=9
laStr[4,3]=5
laStr[5,3]=4
laStr[6,3]=4
laStr[7,3]=30
laStr[8,3]=34
laStr[9,3]=8
laStr[10,3]=14
laStr[11,3]=13
laStr[12,3]=7
laStr[13,3]=9

FOR lnCnt = 1 TO 13
  laStr[lnCnt,4]=0
ENDFOR

DIMENSION laSFields[13] && Array of fields of selected file
DIMENSION laFields[13]  && Array of correct fields of *.CSV file
laFields[1]='Div'
laFields[2]='Se/Yr'
laFields[3]='Style'
laFields[4]='Color'
laFields[5]=''
laFields[6]='Size'
laFields[7]='Description'
laFields[8]='Description2'
laFields[9]='Group'
laFields[10]='Pur. Grp.'
laFields[11]='Selling Price'
laFields[12]='MSRP'
laFields[13]='Retail'

llCanCon = .T.             && Flag to Can Convert the file or not
lcAlias = SELECT()

=gfSetup()

SELECT CODES
lcTmpIndex = gfTempName()   && Temp Index of Code Table
INDEX ON ALLTRIM(CFLD_NAME) + CDISCREP TAG (lcTmpIndex) OF (gcWorkDir+lcTmpIndex) 

lcTmpData = gfTempName()   && Temp Table of converted data
CREATE TABLE (gcWorkDir+lcTmpData) FROM ARRAY laStr

*-- Selecting the file to be convert.
DO WHILE .T.
  lcFile = GETFILE('CSV' , 'Select the file:' , 'Convert')
  IF EMPTY(lcFile)  && No files are selected
    =gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,'Opration Canceled')
    llCanCon = .F.
    EXIT
  ENDIF
  IF RIGHT(lcFile,3)<>'CSV' && the selected file is not *.CSV
    =gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,'The Selected File Must be in format *.CSV')
    LOOP
  ELSE                      && the selected file is *.CSV
    SELECT (lcTmpData)
    ZAP
    APPEND FROM &lcFile DELIMITED
    LOCATE
    SCATTER TO laSFields
    FOR lnCnt = 1 TO 13
      IF laSFields[lnCnt] <> laFields[lnCnt]  && the structure of selected file is not correct
        =gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,'The Structure of Selected File is Not Correct')
        llCanCon = .F.
        EXIT
      ENDIF
    ENDFOR
    IF !llCanCon   && the structure of selected file is not correct
      llCanCon = .T.
      LOOP
    ENDIF
    IF gfModalGen("QRM00000B42002","DIALOG",.F.,.F.,'Are you sure you want to Convert ?')=2
      LOOP
    ENDIF
    EXIT
  ENDIF
ENDDO
IF llCanCon
  = lfConvert()
ENDIF

*-- Closing the Tmp File.
SELECT CODES
SET ORDER TO 
SELECT (lcTmpData)
USE
SELECT (lcAlias)
ERASE (gcWorkDir+lcTmpData+'.*')
ERASE (gcWorkDir+lcTmpIndex+'.*')

*****************************************************************************
*: Program file  : lfConvert  (C# 101933)
*: Program desc. : Function For Converting The Tmp File
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED MAHER - (AMH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfConvert()
*****************************************************************************
*:
FUNCTION lfConvert

DELETE      && Deleting the recorde contined fields names
INDEX ON STYLE TAG STYLE OF (lcTmpData)
lnCount = 0     && Counter of appended recordes
lcOldMajor = ''
SELECT ICISTRU
SEEK 'U1'
lcSepr = Cisegsepr    && Get the Seprator of First Segmant
SELECT (lcTmpData)
lnToBeDone = RECCOUNT()-1
DO WHILE !EOF()
  SELECT STYLE
  APPEND BLANK
  REPLACE STYLE      WITH ALLTRIM(&lcTmpData..Style) + lcSepr + ALLTRIM(&lcTmpData..Color) + ;
                          PADL(ALLTRIM(&lcTmpData..A5),3,'0') ,;
          CSTYMAJOR  WITH &lcTmpData..Style ,;
          CDYE_FLG   WITH 'N',;
          STATUS     WITH 'A',;
          CSTYGROUP  WITH &lcTmpData..Group ,;
          SCALE      WITH IIF(SEEK('S' + &lcTmpData..Size,'SCALE'),&lcTmpData..Size,'E') ,;
          DESC       WITH &lcTmpData..Desc ,;
          DESC1      WITH &lcTmpData..Desc2 ,;
          SEASON     WITH LEFT(&lcTmpData..Color,2) ,;
          CSTYGRADE  WITH '1' ,;
          COMMISSION WITH .T. ,;
          LINK_CODE  WITH IIF(&lcTmpData..Div = '01' , 'DEFDEF' ,;
                          IIF(&lcTmpData..Div = '06' , 'KIDDEF' ,;
                          IIF(&lcTmpData..Div = '08' , 'PLBDEF' , ''))),;
          MAKE       WITH .F. ,;
          CPRICECUR  WITH 'USD' ,;
          CDUTYCUR   WITH 'USD' ,;
          LINVSTY    WITH .T.
  REPLACE CPURCODE   WITH IIF(SEEK('CPURCODE'+&lcTmpData..Pur_Grp,'CODES'),CODES.CCODE_NO,''),;
          PRICEA     WITH VAL(&lcTmpData..Sell_Price) ,;
          CCONSINFO1 WITH 'MSRP' ,;
          NSUGRETPRI WITH VAL(&lcTmpData..Retail) ,;
          CDEFWARE   WITH 'NL' ,;
          CSLSGLLINK WITH 'DEF' ,;
          CDISCCODE  WITH '000009' ,;
          LDETCOST   WITH .T. ,;
          CDIVISION  WITH LEFT(&lcTmpData..Div,2) ,;
          LTAXABLE   WITH .F.
  SELECT STYDYE
  APPEND BLANK
  REPLACE STYLE      WITH ALLTRIM(&lcTmpData..Style) + lcSepr + ALLTRIM(&lcTmpData..Color) + ;
                          PADL(ALLTRIM(&lcTmpData..A5),3,'0') ,;
          CWARECODE  WITH 'NL' ,;
          GL_LINK    WITH IIF(&lcTmpData..Div = '01' , 'DEFDEF' ,;
                          IIF(&lcTmpData..Div = '06' , 'KIDDEF' ,;
                          IIF(&lcTmpData..Div = '08' , 'PLBDEF' , '')))
  IF lcOldMajor <> &lcTmpData..Style
    SELECT ICSEGVAL
    APPEND BLANK
    REPLACE CISEGNO    WITH '1' ,;
            CISEGVAL   WITH &lcTmpData..Style ,;
            CISGVALSD  WITH &lcTmpData..Desc ,;
            CISGVALLD  WITH &lcTmpData..Desc2
    lcOldMajor = &lcTmpData..Style
  ENDIF
  SELECT ICSTYHST
  FOR lnDif = -1 TO 1
    APPEND BLANK
    REPLACE STYLE      WITH ALLTRIM(&lcTmpData..Style) + lcSepr + ALLTRIM(&lcTmpData..Color) + ;
                            PADL(ALLTRIM(&lcTmpData..A5),3,'0') ,;
            CFISFYEAR  WITH ALLTRIM(STR(VAL(gcCurrYear) + lnDif))
  ENDFOR
  *WAIT WINDOW 'Converting Style : ' + lcOldMajor NOWAIT
  lnCount = lnCount + 1
  =gfThermo(lnToBeDone,lnCount,'Converting','Style : ' + lcOldMajor)
  SELECT (lcTmpData)
  SKIP
ENDDO
lcMessage = STR(lnCount) + ' Style/Color are Correctly Converted'
=gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,lcMessage)