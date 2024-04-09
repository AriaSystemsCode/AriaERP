*:***************************************************************************
*: Program file  : ALPKTKFP
*: Program desc. : Custom Picking Ticket Form for Fresh Produce
*! Date          : 07/19/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Ahmed Salah Shalaby - (SSH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKFP
*:***************************************************************************
*: This Report Program is due to C # 200085
*C200137,1 TAK 10/10/2000
*:***************************************************************************

*-- lcTemp represents the temp file to collect the data needed for
*-- the custom form. It will be taken from the standard collected data
*-- plus some added fields
STORE '' TO lcTemp,lcTempHd
lcTemp   = gfTempName()
lcTempHd = gfTempName()
*-- lcTmpOrdL is the standard colleted temp file
SELECT (lcTmpOrdL)
*-- copy structure to array laFileStru
=AFIELDS(laFileStru)           
*-- add needed fields for the custom form
lnFileStru = ALEN(laFileStru,1)
**C200137,1
*DIMENSION laFileStru[lnFileStru + 9, 4]
DIMENSION laFileStru[lnFileStru + 8, 4]
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'CTktNo'  && adornment po#
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cDsgnCode'  && design code
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cNdrpid'  && name drop id
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cstylepos'  && Style Pos.
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Account'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
**C200137,1
*laFileStru[lnFileStru ,1] = 'Store' 
*laFileStru[lnFileStru ,2] = 'C'
*laFileStru[lnFileStru ,3] = 8
*laFileStru[lnFileStru ,4] = 0
*lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'TOTALQTY'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 8
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cLocation' 
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cpicktype'  && adornment po#
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 2
laFileStru[lnFileStru ,4] = 0

CREATE TABLE  (gcWorkDir+lcTemp)  FROM ARRAY laFileStru
=gfOpenFile(gcWorkDir+lcTemp ,'','EX')


*--Create header file for this line.
*---ssh
DIMENSION laFileStr1[3,4]
laFileStr1[1 ,1] = 'CTktNo'  && adornment po#
laFileStr1[1 ,2] = 'C'
laFileStr1[1 ,3] = 6
laFileStr1[1 ,4] = 0
laFileStr1[2 ,1] = 'TotPik'  && design code
laFileStr1[2 ,2] = 'N'
laFileStr1[2 ,3] = 8
laFileStr1[2 ,4] = 0
laFileStr1[3 ,1] = 'cpicktype'  && adornment po#
laFileStr1[3 ,2] = 'C'
laFileStr1[3 ,3] = 2
laFileStr1[3 ,4] = 0

CREATE TABLE  (gcWorkDir+lcTempHd) FROM ARRAY laFileStr1
=gfOpenFile(gcWorkDir+lcTempHd,'','EX')
INDEX ON CTktNo TAG (lcTempHd)

*-- open bomvar, icdesign, icnamdrp, cutpick
STORE .F. TO llBomvar, llDesign, llNamDrp, llCutPick,llPoshdr
IF !llPoshdr AND !USED('POSHDR')
  llPoshdr = gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF

IF !llBomvar AND !USED('BOMVAR')
  llBomvar = gfOpenFile(gcDataDir+'BOMVAR',gcDataDir+'BOMVAR','SH')  && cidtype+ccost_id+STR(lineno,6)
ENDIF
IF !llDesign AND !USED('ICDESIGN')
  llDesign = gfOpenFile(gcDataDir+'ICDESIGN',gcDataDir+'ICDESIGN','SH')  && cdsgncode
ENDIF
IF !llNamDrp AND !USED('ICNAMDRP')
  llNamDrp =  gfOpenFile(gcDataDir+'ICNAMDRP',gcDataDir+'IcNamDrpId','SH')  && cndrpid
ENDIF
IF !llCutPick AND !USED('CUTPICK')
  llCutPick = gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')  &&trancd+order+cordline
ENDIF


*--
*-- scan throught lcTmpOrdL to evaluate each adornment po# (if exist)
*--,design and namedrop
SELECT whsloc
SET ORDER TO Whslocst
SELECT BOMVAR
SET ORDER TO BOMVAR
SELECT CUTPICK
SET ORDER TO CUTORD
SELECT (lcTmpOrdL)
GOTO TOP
*--
SCAN FOR cgrupdetal='D'
  IF SEEK(PikTkt,'PikTkt') AND ALLTRIM(PikTkt.cpicktype) = ALLTRIM(lcRpPkTy) .AND. PikTkt.Status <> 'X'

  ELSE
    LOOP
  ENDIF
  SCATTER MEMVAR MEMO
  =SEEK('O'+m.Order+STR(m.lineno,6),'OrdLine')
  *-- we may have one adornment po per one or several piktkt
  *-- or not yet the adornment po been made, in this case
  *-- we'll collect so adornments
  IF SEEK('2'+ m.Order +STR(m.lineno,6),'CUTPICK')
    lcKey = "'AD' + CUTPICK.cTktNo + PADL(ALLTRIM(cutpick.ctktlineno),6)"
    *m.cpicktype = "A"
  ELSE
    lcKey = "'SO' + &lcTmpOrdL..Order + STR(&lcTmpOrdL..LineNo,6)"
    *m.cpicktype = "O"
  ENDIF
  *-- either adornment po designs/name drops or order designs/name
  IF SEEK(&lcKey,'BOMVAR')
    m.cpicktype = "A"
    SELECT (lcTempHd)
    IF !SEEK(IIF(BOMVAR.cIdType='AD',BOMVAR.cCost_id,&lcTmpOrdL..Piktkt))
      APPEND BLANK
    ENDIF
    REPLACE cTktNo WITH IIF(BOMVAR.cIdType='AD',BOMVAR.cCost_id,&lcTmpOrdL..Piktkt) ,;
            TotPik WITH TotPik + &lcTmpOrdL..TotPik

    SELECT BOMVAR
    SCAN WHILE  cIdType + cCost_Id + STR(lineno,6) = &lcKey
      SELECT (lcTemp)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cTktNo     WITH IIF(BOMVAR.cIdType = 'AD',BOMVAR.cCost_id,&lcTmpOrdL..Piktkt)  ,;
              cDsgnCode  WITH BOMVAR.cDsgnCode ,;
              cNdrpid    WITH BOMVAR.cNdrpid ,;
              cstylepos  WITH Bomvar.cstylepos,;
              Account    WITH ORDLINE.Account,;
              Store      WITH ORDLINE.Store

      lcWhCode = IIF(SEEK('A'+BOMVAR.cCost_Id,'POSHDR'),PADR(POSHDR.Vendor,6),ORDLINE.cWareCode)
      IF SEEK(OrdLine.Style+SPACE(06)+lcWhCode,'whsloc')
         REPLACE cLocation WITH whsloc.cLocation
      ENDIF


    ENDSCAN
  ELSE  && Pick tiket was blank (not adornment).
    m.cpicktype = "B"
    SELECT (lcTempHd)
    IF !SEEK(&lcTmpOrdL..Piktkt)
      APPEND BLANK
    ENDIF
    REPLACE cTktNo WITH &lcTmpOrdL..Piktkt ,;
            TotPik WITH TotPik + &lcTmpOrdL..TotPik
    SELECT (lcTemp)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cTktNo    WITH &lcTmpOrdL..Piktkt,;
            Account   WITH ORDLINE.Account,;
            Store     WITH ORDLINE.Store

    lcWhCode = IIF(SEEK(ORDLINE.Piktkt,'PIKTKT'),PIKTKT.cWareCode,ORDLINE.cWareCode)
    IF SEEK(OrdLine.Style+SPACE(06)+lcWhCode,'whsloc')
       REPLACE cLocation WITH whsloc.cLocation
    ENDIF

  ENDIF
ENDSCAN

SELECT (lcTempHd)
SCAN
  SELECT (lcTemp)
  REPLACE ALL TOTALQTY WITH &lcTempHd..TotPik FOR cTktNo = &lcTempHd..cTktNo
ENDSCAN
SELECT ICNAMDRP
SET ORDER TO IcNamDrpId
SELECT ICDESIGN
SET ORDER TO ICDESIGN

SELECT (lcTemp)
SET RELATION TO
SET RELATION TO Cdsgncode INTO ICDESIGN ADDITIVE
SET RELATION TO Cndrpid INTO ICNAMDRP ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO "O"+order+STR(lineno,6) INTO Ordline ADDITIVE
SET RELATION TO "O"+order INTO ORDHDR ADDITIVE
*SET RELATION TO IIF(EMPTY(Store) , 'M'+Account ,;
                      'S'+Account+Store) INTO CUSTOMER ADDITIVE
INDEX ON cTktNo+Order+PikTkt+cstylepos+cDsgnCode+cNdrpid+cLocation+Style TAG (lcTemp)
SELECT (lcTemp)
GOTO TOP
RETURN





*!*************************************************************
*! Name      : BC_Code39
*! Purpose   : Print BarCode
*!*************************************************************
*! Example   :  bc_code39('000010')
*!*************************************************************
FUNCTION BC_Code39
Parameters lcRet,m

IF EMPTY(m)
  RETURN ''
ENDIF

full  = "Basic"
check = 0
private t,x,p,cval,chktot
set talk off
* Generates character pattern for code 39 bar codes
*    m  = text to decode into a character pattern
*
* Returns:
*    Bar pattern for code 39 bar code.
*    If an illegal value is found then a null will
*    be returned to the user.

*** if the parameter is not a character then quit
if type('m')<>"C"
  return ""
endif
*** verify full parameter
if type('full')<>"C"
  full = "BASIC"
  cval = 2
else
  full = upper(full)
  if full<>"FULL"
    full = "BASIC"
    cval = 2
  else
    cval = 3
  endif
endif

*** verify check parameter
if type('check')<>'N'
  check = 0
else
  if check <> 1
    check = 0
  endif
endif

*** Check to see if user added stop/start
*** character and returns null if true
if at("*"," "+m)>1
  return ""
endif

*** decode message into bar code character pattern and place
*** it into the variable

* initialize the output string and add the start code to it
p="*"

* Dimension the conversion array
dimension cvtarr[4,128]

* Define the conversion array
cvtarr[1,1]=chr(0)
cvtarr[2,1]=""
cvtarr[3,1]="%U"
cvtarr[4,1]=0
cvtarr[1,2]=chr(1)
cvtarr[2,2]=""
cvtarr[3,2]="$A"
cvtarr[4,2]=0
cvtarr[1,3]=chr(2)
cvtarr[2,3]=""
cvtarr[3,3]="$B"
cvtarr[4,3]=0
cvtarr[1,4]=chr(3)
cvtarr[2,4]=""
cvtarr[3,4]="$C"
cvtarr[4,4]=0
cvtarr[1,5]=chr(4)
cvtarr[2,5]=""
cvtarr[3,5]="$D"
cvtarr[4,5]=0
cvtarr[1,6]=chr(5)
cvtarr[2,6]=""
cvtarr[3,6]="$E"
cvtarr[4,6]=0
cvtarr[1,7]=chr(6)
cvtarr[2,7]=""
cvtarr[3,7]="$F"
cvtarr[4,7]=0
cvtarr[1,8]=chr(7)
cvtarr[2,8]=""
cvtarr[3,8]="$G"
cvtarr[4,8]=0
cvtarr[1,9]=chr(8)
cvtarr[2,9]=""
cvtarr[3,9]="$H"
cvtarr[4,9]=0
cvtarr[1,10]=chr(9)
cvtarr[2,10]=""
cvtarr[3,10]="$I"
cvtarr[4,10]=0
cvtarr[1,11]=chr(10)
cvtarr[2,11]=""
cvtarr[3,11]="$J"
cvtarr[4,11]=0
cvtarr[1,12]=chr(11)
cvtarr[2,12]=""
cvtarr[3,12]="$K"
cvtarr[4,12]=0
cvtarr[1,13]=chr(12)
cvtarr[2,13]=""
cvtarr[3,13]="$L"
cvtarr[4,13]=0
cvtarr[1,14]=chr(13)
cvtarr[2,14]=""
cvtarr[3,14]="$M"
cvtarr[4,14]=0
cvtarr[1,15]=chr(14)
cvtarr[2,15]=""
cvtarr[3,15]="$N"
cvtarr[4,15]=0
cvtarr[1,16]=chr(15)
cvtarr[2,16]=""
cvtarr[3,16]="$O"
cvtarr[4,16]=0
cvtarr[1,17]=chr(16)
cvtarr[2,17]=""
cvtarr[3,17]="$P"
cvtarr[4,17]=0
cvtarr[1,18]=chr(17)
cvtarr[2,18]=""
cvtarr[3,18]="$Q"
cvtarr[4,18]=0
cvtarr[1,19]=chr(18)
cvtarr[2,19]=""
cvtarr[3,19]="$R"
cvtarr[4,19]=0
cvtarr[1,20]=chr(19)
cvtarr[2,20]=""
cvtarr[3,20]="$S"
cvtarr[4,20]=0
cvtarr[1,21]=chr(20)
cvtarr[2,21]=""
cvtarr[3,21]="$T"
cvtarr[4,21]=0
cvtarr[1,22]=chr(21)
cvtarr[2,22]=""
cvtarr[3,22]="$U"
cvtarr[4,22]=0
cvtarr[1,23]=chr(22)
cvtarr[2,23]=""
cvtarr[3,23]="$V"
cvtarr[4,23]=0
cvtarr[1,24]=chr(23)
cvtarr[2,24]=""
cvtarr[3,24]="$W"
cvtarr[4,24]=0
cvtarr[1,25]=chr(24)
cvtarr[2,25]=""
cvtarr[3,25]="$X"
cvtarr[4,25]=0
cvtarr[1,26]=chr(25)
cvtarr[2,26]=""
cvtarr[3,26]="$Y"
cvtarr[4,26]=0
cvtarr[1,27]=chr(26)
cvtarr[2,27]=""
cvtarr[3,27]="$Z"
cvtarr[4,27]=0
cvtarr[1,28]=chr(27)
cvtarr[2,28]=""
cvtarr[3,28]="%A"
cvtarr[4,28]=0
cvtarr[1,29]=chr(28)
cvtarr[2,29]=""
cvtarr[3,29]="%B"
cvtarr[4,29]=0
cvtarr[1,30]=chr(29)
cvtarr[2,30]=""
cvtarr[3,30]="%C"
cvtarr[4,30]=0
cvtarr[1,31]=chr(30)
cvtarr[2,31]=""
cvtarr[3,31]="%D"
cvtarr[4,31]=0
cvtarr[1,32]=chr(31)
cvtarr[2,32]=""
cvtarr[3,32]="%E"
cvtarr[4,32]=0
cvtarr[1,33]=chr(32)
cvtarr[2,33]=" "
cvtarr[3,33]=" "
cvtarr[4,33]=38
cvtarr[1,34]="!"
cvtarr[2,34]=""
cvtarr[3,34]="/A"
cvtarr[4,34]=0
cvtarr[1,35]=chr(34)
cvtarr[2,35]=""
cvtarr[3,35]="/B"
cvtarr[4,35]=0
cvtarr[1,36]="#"
cvtarr[2,36]=""
cvtarr[3,36]="/C"
cvtarr[4,36]=0
cvtarr[1,37]="$"
cvtarr[2,37]="$"
cvtarr[3,37]="/D"
cvtarr[4,37]=39
cvtarr[1,38]="%"
cvtarr[2,38]="%"
cvtarr[3,38]="/E"
cvtarr[4,38]=42
cvtarr[1,39]="&"
cvtarr[2,39]=""
cvtarr[3,39]="/F"
cvtarr[4,39]=0
cvtarr[1,40]=chr(39)
cvtarr[2,40]=""
cvtarr[3,40]="/G"
cvtarr[4,40]=0
cvtarr[1,41]="("
cvtarr[2,41]=""
cvtarr[3,41]="/H"
cvtarr[4,41]=0
cvtarr[1,42]=")"
cvtarr[2,42]=""
cvtarr[3,42]="/I"
cvtarr[4,42]=0
cvtarr[1,43]="*"
cvtarr[2,43]=""
cvtarr[3,43]="/J"
cvtarr[4,43]=0
cvtarr[1,44]="+"
cvtarr[2,44]="+"
cvtarr[3,44]="/K"
cvtarr[4,44]=41
cvtarr[1,45]=","
cvtarr[2,45]=""
cvtarr[3,45]="/L"
cvtarr[4,45]=0
cvtarr[1,46]="-"
cvtarr[2,46]="-"
cvtarr[3,46]="-"
cvtarr[4,46]=36
cvtarr[1,47]="."
cvtarr[2,47]="."
cvtarr[3,47]="."
cvtarr[4,47]=37
cvtarr[1,48]="/"
cvtarr[2,48]="/"
cvtarr[3,48]="/O"
cvtarr[4,48]=40
cvtarr[1,49]="0"
cvtarr[2,49]="0"
cvtarr[3,49]="0"
cvtarr[4,49]=0
cvtarr[1,50]="1"
cvtarr[2,50]="1"
cvtarr[3,50]="1"
cvtarr[4,50]=1
cvtarr[1,51]="2"
cvtarr[2,51]="2"
cvtarr[3,51]="2"
cvtarr[4,51]=2
cvtarr[1,52]="3"
cvtarr[2,52]="3"
cvtarr[3,52]="3"
cvtarr[4,52]=3
cvtarr[1,53]="4"
cvtarr[2,53]="4"
cvtarr[3,53]="4"
cvtarr[4,53]=4
cvtarr[1,54]="5"
cvtarr[2,54]="5"
cvtarr[3,54]="5"
cvtarr[4,54]=5
cvtarr[1,55]="6"
cvtarr[2,55]="6"
cvtarr[3,55]="6"
cvtarr[4,55]=6
cvtarr[1,56]="7"
cvtarr[2,56]="7"
cvtarr[3,56]="7"
cvtarr[4,56]=7
cvtarr[1,57]="8"
cvtarr[2,57]="8"
cvtarr[3,57]="8"
cvtarr[4,57]=8
cvtarr[1,58]="9"
cvtarr[2,58]="9"
cvtarr[3,58]="9"
cvtarr[4,58]=9
cvtarr[1,59]=":"
cvtarr[2,59]=""
cvtarr[3,59]="/Z"
cvtarr[4,59]=0
cvtarr[1,60]=";"
cvtarr[2,60]=""
cvtarr[3,60]="%F"
cvtarr[4,60]=0
cvtarr[1,61]="<"
cvtarr[2,61]=""
cvtarr[3,61]="%G"
cvtarr[4,61]=0
cvtarr[1,62]="="
cvtarr[2,62]=""
cvtarr[3,62]="%H"
cvtarr[4,62]=0
cvtarr[1,63]=">"
cvtarr[2,63]=""
cvtarr[3,63]="%I"
cvtarr[4,63]=0
cvtarr[1,64]="?"
cvtarr[2,64]=""
cvtarr[3,64]="%J"
cvtarr[4,64]=0
cvtarr[1,65]="@"
cvtarr[2,65]=""
cvtarr[3,65]="%V"
cvtarr[4,65]=0
cvtarr[1,66]="A"
cvtarr[2,66]="A"
cvtarr[3,66]="A"
cvtarr[4,66]=10
cvtarr[1,67]="B"
cvtarr[2,67]="B"
cvtarr[3,67]="B"
cvtarr[4,67]=11
cvtarr[1,68]="C"
cvtarr[2,68]="C"
cvtarr[3,68]="C"
cvtarr[4,68]=12
cvtarr[1,69]="D"
cvtarr[2,69]="D"
cvtarr[3,69]="D"
cvtarr[4,69]=13
cvtarr[1,70]="E"
cvtarr[2,70]="E"
cvtarr[3,70]="E"
cvtarr[4,70]=14
cvtarr[1,71]="F"
cvtarr[2,71]="F"
cvtarr[3,71]="F"
cvtarr[4,71]=15
cvtarr[1,72]="G"
cvtarr[2,72]="G"
cvtarr[3,72]="G"
cvtarr[4,72]=16
cvtarr[1,73]="H"
cvtarr[2,73]="H"
cvtarr[3,73]="H"
cvtarr[4,73]=17
cvtarr[1,74]="I"
cvtarr[2,74]="I"
cvtarr[3,74]="I"
cvtarr[4,74]=18
cvtarr[1,75]="J"
cvtarr[2,75]="J"
cvtarr[3,75]="J"
cvtarr[4,75]=19
cvtarr[1,76]="K"
cvtarr[2,76]="K"
cvtarr[3,76]="K"
cvtarr[4,76]=20
cvtarr[1,77]="L"
cvtarr[2,77]="L"
cvtarr[3,77]="L"
cvtarr[4,77]=21
cvtarr[1,78]="M"
cvtarr[2,78]="M"
cvtarr[3,78]="M"
cvtarr[4,78]=22
cvtarr[1,79]="N"
cvtarr[2,79]="N"
cvtarr[3,79]="N"
cvtarr[4,79]=23
cvtarr[1,80]="O"
cvtarr[2,80]="O"
cvtarr[3,80]="O"
cvtarr[4,80]=24
cvtarr[1,81]="P"
cvtarr[2,81]="P"
cvtarr[3,81]="P"
cvtarr[4,81]=25
cvtarr[1,82]="Q"
cvtarr[2,82]="Q"
cvtarr[3,82]="Q"
cvtarr[4,82]=26
cvtarr[1,83]="R"
cvtarr[2,83]="R"
cvtarr[3,83]="R"
cvtarr[4,83]=27
cvtarr[1,84]="S"
cvtarr[2,84]="S"
cvtarr[3,84]="S"
cvtarr[4,84]=28
cvtarr[1,85]="T"
cvtarr[2,85]="T"
cvtarr[3,85]="T"
cvtarr[4,85]=29
cvtarr[1,86]="U"
cvtarr[2,86]="U"
cvtarr[3,86]="U"
cvtarr[4,86]=30
cvtarr[1,87]="V"
cvtarr[2,87]="V"
cvtarr[3,87]="V"
cvtarr[4,87]=31
cvtarr[1,88]="W"
cvtarr[2,88]="W"
cvtarr[3,88]="W"
cvtarr[4,88]=32
cvtarr[1,89]="X"
cvtarr[2,89]="X"
cvtarr[3,89]="X"
cvtarr[4,89]=33
cvtarr[1,90]="Y"
cvtarr[2,90]="Y"
cvtarr[3,90]="Y"
cvtarr[4,90]=34
cvtarr[1,91]="Z"
cvtarr[2,91]="Z"
cvtarr[3,91]="Z"
cvtarr[4,91]=35
cvtarr[1,92]="["
cvtarr[2,92]=""
cvtarr[3,92]="%K"
cvtarr[4,92]=0
cvtarr[1,93]="\"
cvtarr[2,93]=""
cvtarr[3,93]="%L"
cvtarr[4,93]=0
cvtarr[1,94]="]"
cvtarr[2,94]=""
cvtarr[3,94]="%M"
cvtarr[4,94]=0
cvtarr[1,95]="^"
cvtarr[2,95]=""
cvtarr[3,95]="%N"
cvtarr[4,95]=0
cvtarr[1,96]="_"
cvtarr[2,96]=""
cvtarr[3,96]="%O"
cvtarr[4,96]=0
cvtarr[1,97]=chr(96)
cvtarr[2,97]=""
cvtarr[3,97]="%W"
cvtarr[4,97]=0
cvtarr[1,98]="a"
cvtarr[2,98]=""
cvtarr[3,98]="+A"
cvtarr[4,98]=0
cvtarr[1,99]="b"
cvtarr[2,99]=""
cvtarr[3,99]="+B"
cvtarr[4,99]=0
cvtarr[1,100]="c"
cvtarr[2,100]=""
cvtarr[3,100]="+C"
cvtarr[4,100]=0
cvtarr[1,101]="d"
cvtarr[2,101]=""
cvtarr[3,101]="+D"
cvtarr[4,101]=0
cvtarr[1,102]="e"
cvtarr[2,102]=""
cvtarr[3,102]="+E"
cvtarr[4,102]=0
cvtarr[1,103]="f"
cvtarr[2,103]=""
cvtarr[3,103]="+F"
cvtarr[4,103]=0
cvtarr[1,104]="g"
cvtarr[2,104]=""
cvtarr[3,104]="+G"
cvtarr[4,104]=0
cvtarr[1,105]="h"
cvtarr[2,105]=""
cvtarr[3,105]="+H"
cvtarr[4,105]=0
cvtarr[1,106]="i"
cvtarr[2,106]=""
cvtarr[3,106]="+I"
cvtarr[4,106]=0
cvtarr[1,107]="j"
cvtarr[2,107]=""
cvtarr[3,107]="+J"
cvtarr[4,107]=0
cvtarr[1,108]="k"
cvtarr[2,108]=""
cvtarr[3,108]="+K"
cvtarr[4,108]=0
cvtarr[1,109]="l"
cvtarr[2,109]=""
cvtarr[3,109]="+L"
cvtarr[4,109]=0
cvtarr[1,110]="m"
cvtarr[2,110]=""
cvtarr[3,110]="+M"
cvtarr[4,110]=0
cvtarr[1,111]="n"
cvtarr[2,111]=""
cvtarr[3,111]="+N"
cvtarr[4,111]=0
cvtarr[1,112]="o"
cvtarr[2,112]=""
cvtarr[3,112]="+O"
cvtarr[4,112]=0
cvtarr[1,113]="p"
cvtarr[2,113]=""
cvtarr[3,113]="+P"
cvtarr[4,113]=0
cvtarr[1,114]="q"
cvtarr[2,114]=""
cvtarr[3,114]="+Q"
cvtarr[4,114]=0
cvtarr[1,115]="r"
cvtarr[2,115]=""
cvtarr[3,115]="+R"
cvtarr[4,115]=0
cvtarr[1,116]="s"
cvtarr[2,116]=""
cvtarr[3,116]="+S"
cvtarr[4,116]=0
cvtarr[1,117]="t"
cvtarr[2,117]=""
cvtarr[3,117]="+T"
cvtarr[4,117]=0
cvtarr[1,118]="u"
cvtarr[2,118]=""
cvtarr[3,118]="+U"
cvtarr[4,118]=0
cvtarr[1,119]="v"
cvtarr[2,119]=""
cvtarr[3,119]="+V"
cvtarr[4,119]=0
cvtarr[1,120]="w"
cvtarr[2,120]=""
cvtarr[3,120]="+W"
cvtarr[4,120]=0
cvtarr[1,121]="x"
cvtarr[2,121]=""
cvtarr[3,121]="+X"
cvtarr[4,121]=0
cvtarr[1,122]="y"
cvtarr[2,122]=""
cvtarr[3,122]="+Y"
cvtarr[4,122]=0
cvtarr[1,123]="z"
cvtarr[2,123]=""
cvtarr[3,123]="+Z"
cvtarr[4,123]=0
cvtarr[1,124]="{"
cvtarr[2,124]=""
cvtarr[3,124]="%P"
cvtarr[4,124]=0
cvtarr[1,125]="|"
cvtarr[2,125]=""
cvtarr[3,125]="%Q"
cvtarr[4,125]=0
cvtarr[1,126]="}"
cvtarr[2,126]=""
cvtarr[3,126]="%R"
cvtarr[4,126]=0
cvtarr[1,127]="~"
cvtarr[2,127]=""
cvtarr[3,127]="%S"
cvtarr[4,127]=0
cvtarr[1,128]=chr(127)
cvtarr[2,128]=""
cvtarr[3,128]="%T"
cvtarr[4,128]=0

chktot = 0
*** process the message
FOR x = 1 TO len(m)
  t = substr(m, x, 1)
  rtnc = asubscript(cvtarr,ascan(cvtarr,t,aelement(cvtarr,1,1),128),2)
  if rtnc <> 0
    p = p + cvtarr[cval,rtnc]
    chktot = chktot + cvtarr[4,rtnc]
  endif
endfor

if check = 1
  chkstr = 43 - (chktot % 43)
  rtnc = asubscript(cvtarr,ascan(cvtarr,chkstr,aelement(cvtarr,4,1),128),2)
  if rtnc <> 0
    p = p + cvtarr[1,rtnc]
  endif
endif

rtnp = ALLT(p) + "*"

*** Return the bit pattern
lcRet = rtnp


FUNCTION lfPer
PARAMETER lnDum,lc

*(ORDHDR.nNetAloqty / ORDHDR.book) * 100)

PRIVATE lnAlias,lnTotAlo,lnTotBok
lcOrd1 = Order
lcPk1 = PikTkt
lnTotAlo = 0
lnTotBok = 0
lnAlias = SELECT(0)
SELECT (lnAlias)
=SEEK('O'+Order,'OrdLine')
SELECT ORDLINE
SCAN REST WHILE  cordtype+order+STR(lineno,6) = 'O'+lcOrd1;
          FOR    Picked AND PIKTKT<> '******' AND PikTkt = lcPk1
   lnTotAlo = lnTotAlo+TotPik
   lnTotBok = lnTotBok+TotQty
ENDSCAN
SELECT(lnAlias)
IF lnTotBok = 0
  RETURN lnTotBok
ENDIF
lnDum = ((lnTotAlo/lnTotBok)*100)



FUNCTION lfNotPrtF
Parameters lcRet,lcPik

IF EMPTY(lcPik)
  RETURN
ENDIF
IF SEEK(lcPik,'PIKTKT') .AND. gcDevice <> 'SCREEN'
  REPLACE PIKTKT.PrtFlag WITH 'P'
ENDIF
lcRet = ''
RETURN lcRet