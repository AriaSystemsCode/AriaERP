*:************************************************************************
*: Program file  : POBARCD.PRG
*: Program desc. : Print Bar Codes for receiving P/Os.
*:         System: ARIA APPAREL SYSTEM 4xp
*:         Module: Purchase Orders (PO).
*:      Developer: Mariam Mazhar [MMT]
*:           Date: 06/13/2006 (037705)
*:************************************************************************
loOgScroll.cCRorientation = 'P'
lcStTime = TIME()
IF llFisrt

  loDBFICISTRU = CreateObject("RemoteTable",'ICISTRU','SEGNO','ICISTRU',SET("DATASESSION"))
  loDBFPOSHDR  = CreateObject("RemoteTable",'POSHDR' ,'POSHDR','POSHDR',SET("DATASESSION"))
  loDBFPOSLN   = CreateObject("RemoteTable",'POSLN'  ,'POSLN','POSLN',SET("DATASESSION"))
  loDBFSCALE   = CreateObject("RemoteTable",'SCALE'  ,'SCALE','SCALE',SET("DATASESSION"))
  loDBFSTYLE   = CreateObject("RemoteTable",'STYLE'  ,'STYLE','STYLE',SET("DATASESSION"))

  loDBFICISTRU.Seek('')
  I=1
  SELECT ICISTRU
  SCAN FOR cItemRecTy='U' AND !EMPTY(cISegSepr)
    DIME laSep[I]
    laSep[I]=cISegSepr
    I=I+1
  ENDSCAN 

  lcStyPct = gfItemMask('PI')
  lnStyLen = LEN(lcStyPct)
  llFisrt = .F.

ENDIF 

llLibInst = .T.

llPrntFrm = .T.

IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data......." NOWAIT 
  lfCreateTemp ()
  lfCollectData()
ENDIF 

SELECT (lcTmpBars)
SET ORDER TO  
LOCATE 
IF EOF()
  IF lcRpBasdOn = 'R'
    =gfModalGen('TRM34006B34000','DIALOG','The selected'+'|'+IIF(lcPoTyp='M', 'Cutting tickets' , IIF(lcPoTyp='P','Purchase orders','Inter company purchase orders')))
    RETURN  
  ELSE
    =gfModalGen('TRM00052B00000','DIALOG') 
    RETURN 
  ENDIF
ELSE

=lfOptProg()

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

IF !llPrntFrm
  RETURN
ENDIF

  *--Start printing the bar code labels.
  IF llLibInst
    DO gfDispRe WITH EVAL('lcRpRName')  
  ELSE
    *--Lenght between labels.
    lnLblWd   = 5
    *--No of rows printed counter.
    lnRowPrnt = 1
    *--Scan for selected P/O style/size.
    lcPdSETup = _PDSETUP
    _PDSETUP = ""
    _PDDRIVER = ""
    SELECT (lcTmpBars)
    DO WHILE !EOF()
      lnLblAcross = 1

      *--Variabels to print.
      STORE ' ' TO lcStyCd1,lcStyCd2,lcStyCd3
      STORE ' ' TO lcZDesc1,lcZDesc2,lcZDesc3
      STORE ' ' TO lcSDesc1,lcSDesc2,lcSDesc3
      SCAN WHILE !EOF() AND lnLblAcross <= 3
        lcLbAsNo = STR(lnLblAcross,1) 
        lcStyCd&lcLbAsNo = PADR(cStyle,lnStyLen+1)
        lcZDesc&lcLbAsNo = PADR(ccode,lnStyLen)+' '+SizeDesc
        lcSDesc&lcLbAsNo = PADR(SDesc,23)
        lnLblAcross = lnLblAcross + 1
      ENDSCAN

      *--Call Print bar code.
      =lfPrintbCode()

      *--Check next record.
      IF EOF()
        EXIT
      ENDIF  
    ENDDO
    *--Reset page after printing.
    SET DEVICE TO PRINTER
    @ 0,0 SAY ''
    ??? SPACE(0)
    EJECT PAGE
    SET DEVICE TO SCREEN
    _PDSETUP = lcPdSETup
  ENDIF

ENDIF

SET DEVICE TO SCREEN

RETURN


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/13/2006
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
RETURN

*!*************************************************************
*! Name      : lfRemSepr
*! Developer : Timour A. K.
*! Date      : 05/25/98
*! Purpose   : Function to remove separators from style code.
*!*************************************************************
*! Passed Parameters  : Style with separators.
*!*************************************************************
*! Return : Style without separators.
*!*************************************************************
*! Example   : lcRetSty=lfRemSepr(lcStyle)
*!*************************************************************
FUNCTION lfRemSepr
PARA lcStySpr

*--Replace separators with space(0).
FOR I=1 TO ALEN(laSep)
  lcStySpr=STRTRAN(lcStySpr,laSep,'')
ENDFOR
RETURN lcStySpr

  
*!*************************************************************
*! Name      : lfPrintbCode
*! Developer : Timour A. K.
*! Date      : 05/25/98
*! Purpose   : Print bar code
*! (This function coped from FoxPro applications functions)
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*!          Calls: SET_LASER      (FOXBAR procedure)
*!               : DEF_CODE       (FOXBAR procedure)
*!               : lfBAR_39()     (FOXBAR function )
*!*************************************************************
*! Example   : =lfPrintbCode()
*!*************************************************************
FUNCTION lfPrintbCode

SET ECHO OFF
PUBLIC x_hi,s_bar,across,l_width,l_len,hps(10),lxtra

s_bar   = 2              && DENSITY MODE.
x_hi    = 3              && LABEL HEIGHT LPI  DEFAULT WAS 4.
l_len   = 25             && LABEL LENGTH.
l_width = 6              && LABEL WIDTH
lxtra1   = 5              && EXTRA SPACING BETWEEN LABELS.
*lxtra = ROUND ( ( (LEN(codetext) * hps(s_bar)) / .1 ) ,0)
lxtra   = 3

across  = 3              && LABELS ACROSS. 
IF EMPTY(lcStyCd3)
  across  = 2
ENDIF
IF EMPTY(lcStyCd2)
  across  = 1
ENDIF

hps(1) = .9
hps(2) = .10
hps(3) = .14
hps(4) = .18
hps(5) = .2275
hps(6) = .28


x_esc   = CHR(27)
x_null  = ""
lnLblWd = IIF(lnLblWd=4,5,4)

DO set_laser

DO def_code


SET DEVICE TO PRINTER

*************************** WRITE A BAR CODE LINE **************************
??? SPACE(3)
FOR kk = 1 TO across
  Z=STR(KK,1)
  x_code  = UPPER(ALLTRIM(lcStyCd&Z))
  codetext= "*"+x_code+"*"
  ll = MAX(ROUND ( (l_len - ((LEN(codetext) * hps(s_bar))/.1)),0),0)
  
  bar_out = lfbar_39(codetext)

  ??? bar_out
  ??? SPACE(ll+lxtra1)
ENDFOR

FOR kk = 1 TO x_hi
  ??? CHR(10) + CHR(13)
ENDFOR

*************************** WRITE FIRST TEXT LINE **************************

??? SPACE(3)
FOR kk = 1 TO across
  Z=STR(KK,1)
  x_code  = UPPER(ALLTRIM(lcStyCd&Z))
  codetext= "*"+x_code+"*"
  ll = MAX(ROUND ( (l_len - ((LEN(codetext) * hps(s_bar))/.1)),0),0)  

  ??? '(s16.6H'+PADR(lcZDesc&Z,20*1.6)+'(s10H'
  ??? SPACE(ll+lxtra)
ENDFOR

??? CHR(10) + CHR(13)   
??? SPACE(3)

*************************** WRITE SECOND TEXT LINE **************************
FOR kk = 1 TO across
  Z=STR(KK,1)
  x_code  = UPPER(ALLTRIM(lcStyCd&Z))
  codetext= "*"+x_code+"*"
  ll = MAX(ROUND ( (l_len - ((LEN(codetext) * hps(s_bar))/.1)),0),0)  

  ??? '(s16.6H'+PADR(lcSDesc&Z,20*1.6)+'(s10H'
  ??? SPACE(ll+lxtra)      
ENDFOR

lnRowPrnt = lnRowPrnt + 1

IF lnRowPrnt = 9
   ??? CHR(10) + CHR(13)
   lnRowPrnt = 1
ELSE
  ??? SPACE(3)
  FOR kk = 1 TO lnLblWd  && 4 OR 5   
    ??? CHR(10) + CHR(13)
  ENDFOR
ENDIF

??? SPACE(0)

SET DEVICE TO SCREEN
RETURN


*!*************************************************************
*! Name      : lfBAR_39
*! Developer : Timour A. K.
*! Date      : 05/25/98
*! Purpose   : Read bar code char.
*!*************************************************************
*! Example   : =lfBAR_39()
*!*************************************************************
FUNCTION lfBAR_39
PARAMETERS x_text

x_code = ""
FOR I = 1 TO LEN(x_text)
  x_letter = SUBSTR(x_text,I,1)
  x_code = x_code + IIF(AT(x_letter,chars)=0,x_letter,char[AT(x_letter,chars)]) + ns
ENDFOR
x_code = start + x_code + END
RETURN x_code



*!*********************************************************************
*!
*!      Procedure: SET_LASER
*!
*!*********************************************************************
PROCEDURE set_laser
PUBLIC nb,wb,ns,ws,start,END
dpl   = 50                           && dots per line 300dpi/6lpi = 50dpl
LNBAR = S_BAR*.78
w_bar = ROUND(LNBAR* 2.25,0)      && 2.25 x s_bar
wb = x_esc+"*c"+TRANSFORM(w_bar,'99')+"a"+ALLTRIM(STR(x_hi*dpl))+"b0P"+x_esc+"*p+"+TRANSFORM(w_bar,'99')+"X"
nb = x_esc+"*c"+TRANSFORM(s_bar,'99')+"a"+ALLTRIM(STR(x_hi*dpl))+"b0P"+x_esc+"*p+"+TRANSFORM(s_bar,'99')+"X"
ns = x_esc+"*p+"+TRANSFORM(s_bar,'99')+"X"
ws = x_esc+"*p+"+TRANSFORM(w_bar,'99')+"X"
* adjust cusor position to start at top of line and RETURN to bottom of line
start = x_esc+"*p-50Y"
END   = x_esc+"*p+50Y"
RETURN




*!*********************************************************************
*!
*!      Procedure: DEF_CODE
*!
*!*********************************************************************
PROCEDURE def_code
PUBLIC char[44], chars
DIMENSION char(44)
chars = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%"
char[01] = wb+ns+nb+ws+nb+ns+nb+ns+wb       && Character "1"
char[02] = nb+ns+wb+ws+nb+ns+nb+ns+wb       && Character "2"
char[03] = wb+ns+wb+ws+nb+ns+nb+ns+nb       && Character "3"
char[04] = nb+ns+nb+ws+wb+ns+nb+ns+wb       && Character "4"
char[05] = wb+ns+nb+ws+wb+ns+nb+ns+nb       && Character "5"
char[06] = nb+ns+wb+ws+wb+ns+nb+ns+nb       && Character "6"
char[07] = nb+ns+nb+ws+nb+ns+wb+ns+wb       && Character "7"
char[08] = wb+ns+nb+ws+nb+ns+wb+ns+nb       && Character "8"
char[09] = nb+ns+wb+ws+nb+ns+wb+ns+nb       && Character "9"
char[10] = nb+ns+nb+ws+wb+ns+wb+ns+nb       && Character "0"
char[11] = wb+ns+nb+ns+nb+ws+nb+ns+wb       && Character "A"
char[12] = nb+ns+wb+ns+nb+ws+nb+ns+wb       && Character "B"
char[13] = wb+ns+wb+ns+nb+ws+nb+ns+nb       && Character "C"
char[14] = nb+ns+nb+ns+wb+ws+nb+ns+wb       && Character "D"
char[15] = wb+ns+nb+ns+wb+ws+nb+ns+nb       && Character "E"
char[16] = nb+ns+wb+ns+wb+ws+nb+ns+nb       && Character "F"
char[17] = nb+ns+nb+ns+nb+ws+wb+ns+wb       && Character "G"
char[18] = wb+ns+nb+ns+nb+ws+wb+ns+nb       && Character "H"
char[19] = nb+ns+wb+ns+nb+ws+wb+ns+nb       && Character "I"
char[20] = nb+ns+nb+ns+wb+ws+wb+ns+nb       && Character "J"
char[21] = wb+ns+nb+ns+nb+ns+nb+ws+wb       && Character "K"
char[22] = nb+ns+wb+ns+nb+ns+nb+ws+wb       && Character "L"
char[23] = wb+ns+wb+ns+nb+ns+nb+ws+nb       && Character "M"
char[24] = nb+ns+nb+ns+wb+ns+nb+ws+wb       && Character "N"
char[25] = wb+ns+nb+ns+wb+ns+nb+ws+nb       && Character "O"
char[26] = nb+ns+wb+ns+wb+ns+nb+ws+nb       && Character "P"
char[27] = nb+ns+nb+ns+nb+ns+wb+ws+wb       && Character "Q"
char[28] = wb+ns+nb+ns+nb+ns+wb+ws+nb       && Character "R"
char[29] = nb+ns+wb+ns+nb+ns+wb+ws+nb       && Character "S"
char[30] = nb+ns+nb+ns+wb+ns+wb+ws+nb       && Character "T"
char[31] = wb+ws+nb+ns+nb+ns+nb+ns+wb       && Character "U"
char[32] = nb+ws+wb+ns+nb+ns+nb+ns+wb       && Character "V"
char[33] = wb+ws+wb+ns+nb+ns+nb+ns+nb       && Character "W"
char[34] = nb+ws+nb+ns+wb+ns+nb+ns+wb       && Character "X"
char[35] = wb+ws+nb+ns+wb+ns+nb+ns+nb       && Character "Y"
char[36] = nb+ws+wb+ns+wb+ns+nb+ns+nb       && Character "Z"
char[37] = nb+ws+nb+ns+nb+ns+wb+ns+wb       && Character "-"
char[38] = wb+ws+nb+ns+nb+ns+wb+ns+nb       && Character "."
char[39] = nb+ws+wb+ns+nb+ns+wb+ns+nb       && Character " "
char[40] = nb+ws+nb+ns+wb+ns+wb+ns+nb       && Character "*"
char[41] = nb+ws+nb+ws+nb+ws+nb+ns+nb       && Character "$"
char[42] = nb+ws+nb+ws+nb+ns+nb+ws+nb       && Character "/"
char[43] = nb+ws+nb+ns+nb+ws+nb+ws+nb       && Character "+"
char[44] = nb+ns+nb+ws+nb+ws+nb+ws+nb       && Character "%"
RETURN







*!*************************************************************
*! Name      : BC_Code39
*! Purpose   : Print BarCode
*!*************************************************************
*! Example   :  bc_code39('000010')
*!*************************************************************
Procedure BC_Code39
Parameters m

IF !EMPTY(m)
  *Call the option procedure with basic code39 and no check character
  rtnp = BC_OCode39(ALLT(m),"Basic",0)
  RETURN rtnp
ELSE
  RETURN ''
ENDIF

Procedure BC_OCode39
Parameters m,full,check
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

p = ALLT(p) + "*"

*** Return the bit pattern
return p

*!*************************************************************
*! Name      : lfvTranNo
*! Developer : Reham Al-Allamy
*! Date      : 11/16/1999
*! Purpose   : Valid function of the transaction #
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*!          Calls: None
*!*************************************************************
*! Example   : =lfvTranNo()
*!*************************************************************
*
Function lfvTranNo
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

IF lcPoTyp = "M"
  *-- Browse the cutting tickets from the cutting ticket file.
  IF !EMPTY(lcObj) AND !SEEK(lcObj , 'CUTTKTH')
    SELECT CUTTKTH
    DIMENSION laTemp[1]
    laTemp = ''
    lcBrFields = "Cuttkt :R:H='CutTkt#':8,Style :R:20, "+;
                 "Status :R:H='S':2 , Entered :R:12 , "+;
                 "Complete :R:12 , Pcs_bud :R:H='Tot.Qty':9 ,"+;
                 "Pcs_rec :R:H='Receive':9 , Pcs_opn :R:H='Open':9"
    
    lcFile_Ttl = "Cutting Tickets"
    lcBrowCond = [FOR Pcs_rec>0]
    = gfBrows(lcBrowCond , 'CutTkt' , 'laTemp')
    
    IF !EMPTY(laTemp[1])
      lcObj = laTemp[1]
    ELSE
      lcObj = ""
    ENDIF
  ENDIF
  &lcVar = lcObj
ELSE
  *-- Browse the purchase orders from the purchase orders file.
  IF !EMPTY(lcObj) AND !SEEK(lcObj , 'POSHDR')
    SELECT POSHDR
    DIMENSION laTemp[1]
    laTemp = ''
    lcBrFields = "PO :R :H='P/O #':8,Status :R :H='S':2, "+;
               "Vendor :R :H=IIF(lcPoTyp='N','Source Loc.','Vendor'):12 , "+;
               "Entered :R :H='Entered':12,Complete :R :H='Complete':12 , "+;
               "nStyOrder :R :H='Tot.Qty.':9,POTotal :R :H='Amount':12 , "+;
               "Receive :R :H='Receive':9,Open :R :H='Open':9"
    
    lcFile_Ttl = "Purchse orders"
    lcBrowCond = [lcPoTyp FOR Receive>0]
    = gfBrows(lcBrowCond,'PO','laTemp')
      
    IF !EMPTY(laTemp[1])
      lcObj = laTemp[1]
    ELSE
      lcObj = ""
    ENDIF
  ENDIF
  &lcVar = lcObj
ENDIF

*!*************************************************************
*! Name      : lfvTranTyp
*! Developer : Reham Al-Allamy
*! Date      : 11/17/1999
*! Purpose   : Valid function of the transaction type
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*!          Calls: None
*!*************************************************************
*! Example   : =lfvTranTyp()
*!*************************************************************
*
FUNCTION lfvTranTyp
CLEARREAD()

*!*************************************************************
*! Name      : lfvPRecBdg
*! Developer : Khalid Mohi El-Din M.
*! Date      : 02/05/2001
*! Purpose   : Valid function of the print based on option
*!*************************************************************
*E301545,1 KHM 02/05/2001 
*!*************************************************************

FUNCTION lfvPRecBdg
CLEARREAD()

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/13/2006
*! Purpose   : Function to collect data 
*!*************************************************************
FUNCTION lfCollectData

llRdate = .F.
llTdate = .F.
Ldate = {}
Hdate = {}

IF lcRpBasdOn = 'R'
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSLN.DATE'),1)
  IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
    Ldate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
    Hdate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12,21))
    llRdate  = .T.
  ENDIF
ELSE
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.ENTERED'),1)
  IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
    Ldate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
    Hdate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12,21))
    llTdate = .T.
  ENDIF
ENDIF 


llUsePo = .F.
lcPOFile  = ""
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,IIF(lcPoTyp = "M",'POSHDR.PO1',IIF(lcPoTyp = "P",'POSHDR.PO','POSHDR.PO2'))),1)
IF lnPosition > 0
  lcPOFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUsePo  = IIF(!EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. RECCOUNT(lcPOFile) > 0,.T.,.F.)
ENDIF
lnLbCount = 1  
IF llUsePo
  SELECT (lcPOFile)
  SCAN 
    IF loDBFPOSHDR.Seek(IIF(lcPoTyp = "M",'PU',IIF(lcPoTyp = "P",'PP','NN'))+&lcPOFile..PO) and loDBFPOSLN.Seek(IIF(lcPoTyp = "M",'PU',IIF(lcPoTyp = "P",'PP','NN'))+&lcPOFile..PO)
      SELECT Posln 
      WAIT WINDOW 'Collecting Data.......' nOWAIT 
      SCAN REST WHILE cbusdocu+cstytype+po = IIF(lcPoTyp = "M",'PU',IIF(lcPoTyp = "P",'PP','NN'))+&lcPOFile..PO ;
           FOR IIF(lcRpBasdOn = 'R',TranCd = '2',TranCd = '1')  AND IIF(lcRpBasdOn = 'R' AND llRdate,BETWEEN(POSLN.DATE,Ldate,Hdate),IIF(lcRpBasdOn = 'B' AND llTdate,BETWEEN(POSHDR.ENTERED,Ldate,Hdate),.T.));
           AND Poshdr.Status <> 'X' AND IIF(lcrpbasdon = "R",POSHDR.Receive > 0,.T.)
*        WAIT WINDOW 'P/O#:'+ Posln.PO +'  Style:'+ Style NOWAIT
        lcStyle = PADR(POSLN.Style,lnStyLen)
        lcStyCd = lfRemSepr(lcStyle)
        lcSDesc = IIF(loDBFSTYLE.SEEK(lcStyle,'STYLE'),STYLE.Desc,'')
        lcStyPr = ALLTRIM(STR(STYLE.nSugRetPri,6,2))
        loDBFSCALE.SEEK('S'+STYLE.Scale,'SCALE')
        FOR I=1 TO SCALE.Cnt
          lcSz = STR(I,1)
          IF Posln.Qty&lcSz > 0
            lcZDesc = SCALE.SZ&lcSz 
            lcStySz = lcStyle + lcSz
            *--Increase qty with process persentage.
            lnQty   = CEILING(POSLN.Qty&lcSz * (lnRPPerc/100))
            SELECT (lcTmpBars)
            FOR lnQtyCount = 1 TO lnQty
              
                IF lnLBCount =1
                  APPEND BLANK
                ENDIF
                DO CASE
                  CASE lnlbCount =1
                    REPLACE cCode    WITH lcStySz,;
                            cStyle   WITH lcStyCd+lcSz,; 
                            SDesc    WITH lcSDesc,;
                            SizeDesc WITH lcZDesc,;
                            cPrice   WITH IIF(llPrintPrc,lcStyPr,'') 
                  CASE lnlbCount =2
                    REPLACE cCode1    WITH lcStySz,;
                            cStyle1   WITH lcStyCd+lcSz,; 
                            SDesc1    WITH lcSDesc,;
                            SizeDesc1 WITH lcZDesc,;
                            cPrice1   WITH IIF(llPrintPrc,lcStyPr,'') 
                  CASE lnlbCount =3          
                    REPLACE cCode2    WITH lcStySz,;
                            cStyle2   WITH lcStyCd+lcSz,; 
                            SDesc2    WITH lcSDesc,;
                            SizeDesc2 WITH lcZDesc,;
                            cPrice2   WITH IIF(llPrintPrc,lcStyPr,'') 

                ENDCASE
                lnlbCount = lnlbCount  + 1
                IF lnlbCount = 4
                  lnlbCount = 1
                ENDIF
              ENDFOR
            ENDIF
          ENDFOR  
      ENDSCAN 
    ENDIF 
  ENDSCAN 
ELSE
  IF lcRpBasdOn = 'R' AND llRdate
    IF loDBFPOSLN.Sqlrun("Select POSLN.CSTYTYPE,POSLN.CBUSDOCU ,POSLN.style,POSLN.PO,"+;
                           "posln.qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,trancd,Posln.cinvtype,posln.[lineno]"+;
                           " FROM POSLN(INDEX = POSLN) INNER JOIN POSHDR(INDEX = POSHDR) "+;
                      " ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.CSTYTYPE =POSHDR.CSTYTYPE AND "+;
                      " POSLN.PO = POSHDR.PO WHERE POSLN.CBUSDOCU = '"+IIF(lcPoTyp = "M",'P',IIF(lcPoTyp = "P",'P','N'))+;
                      "' AND POSLN.CSTYTYPE = '"+IIF(lcPoTyp = "M",'U',IIF(lcPoTyp = "P",'P','N'))+"' AND Poshdr.Status <> 'X' AND "+;
                      IIF(lcRpBasdOn = 'R'," TranCd = '2' AND POSHDR.Receive > 0 "," TranCd = '1'")+;
                      IIF(llRdate," AND POSLN.DATE BETWEEN '"+DTOC(Ldate)+"' And '"+DTOC(Hdate)+"'",""))
        *Poshdr.Status <> 'X' AND IIF(lcrpbasdon = "R",POSHDR.Receive > 0,.T.)
      SELECT Posln 
      WAIT WINDOW 'Collecting Data.......' nOWAIT 
      SCAN 
*        WAIT WINDOW 'P/O#:'+ Posln.PO +'  Style:'+ Style NOWAIT
         lcStyle = PADR(POSLN.Style,lnStyLen)
        lcStyCd = lfRemSepr(lcStyle)
        lcSDesc = IIF(loDBFSTYLE.SEEK(lcStyle,'STYLE'),STYLE.Desc,'')
        lcStyPr = ALLTRIM(STR(STYLE.nSugRetPri,6,2))
        loDBFSCALE.SEEK('S'+STYLE.Scale,'SCALE')
        FOR I=1 TO SCALE.Cnt
          lcSz = STR(I,1)
          IF Posln.Qty&lcSz > 0
            lcZDesc = SCALE.SZ&lcSz 
            lcStySz = lcStyle + lcSz
            *--Increase qty with process persentage.
            lnQty   = CEILING(POSLN.Qty&lcSz * (lnRPPerc/100))
            SELECT (lcTmpBars)
            FOR lnQtyCount = 1 TO lnQty
              
                IF lnLBCount =1
                  APPEND BLANK
                ENDIF
                DO CASE
                  CASE lnlbCount =1
                    REPLACE cCode    WITH lcStySz,;
                            cStyle   WITH lcStyCd+lcSz,; 
                            SDesc    WITH lcSDesc,;
                            SizeDesc WITH lcZDesc,;
                            cPrice   WITH IIF(llPrintPrc,lcStyPr,'') 
                  CASE lnlbCount =2
                    REPLACE cCode1    WITH lcStySz,;
                            cStyle1   WITH lcStyCd+lcSz,; 
                            SDesc1    WITH lcSDesc,;
                            SizeDesc1 WITH lcZDesc,;
                            cPrice1   WITH IIF(llPrintPrc,lcStyPr,'') 
                  CASE lnlbCount =3          
                    REPLACE cCode2    WITH lcStySz,;
                            cStyle2   WITH lcStyCd+lcSz,; 
                            SDesc2    WITH lcSDesc,;
                            SizeDesc2 WITH lcZDesc,;
                            cPrice2   WITH IIF(llPrintPrc,lcStyPr,'') 

                ENDCASE
                lnlbCount = lnlbCount  + 1
                IF lnlbCount = 4
                  lnlbCount = 1
                ENDIF
              ENDFOR
            ENDIF
          ENDFOR  
      ENDSCAN 
    ENDIF                   
  ELSE
    IF lcRpBasdOn = 'B' AND llTdate
      IF loDBFPOSLN.Sqlrun("Select POSLN.CSTYTYPE,POSLN.CBUSDOCU ,POSLN.style,POSLN.PO,"+;
                           "posln.qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,trancd,Posln.cinvtype,posln.[lineno]"+;
                           " FROM POSLN(INDEX = POSLN) INNER JOIN POSHDR(INDEX = POSHDR) "+;
                           " ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.CSTYTYPE =POSHDR.CSTYTYPE AND "+;
                           " POSLN.PO = POSHDR.PO WHERE POSLN.CBUSDOCU = '"+IIF(lcPoTyp = "M",'P',IIF(lcPoTyp = "P",'P','N'))+;
                           "' AND POSLN.CSTYTYPE = '"+IIF(lcPoTyp = "M",'U',IIF(lcPoTyp = "P",'P','N'))+"' AND Poshdr.Status <> 'X' AND "+;
                           IIF(lcRpBasdOn = 'R'," TranCd = '2' AND POSHDR.Receive > 0 "," TranCd = '1'")+;
                           IIF(llTdate," AND POSHDR.ENTERED BETWEEN '"+DTOC(Ldate)+"' AND '"+DTOC(Hdate)+"'",""))
        
      SELECT Posln 
      WAIT WINDOW 'Collecting Data.......' nOWAIT 
      SCAN 
*        WAIT WINDOW 'P/O#:'+ Posln.PO +'  Style:'+ Style NOWAIT
        lcStyle = PADR(POSLN.Style,lnStyLen)
        lcStyCd = lfRemSepr(lcStyle)
        lcSDesc = IIF(loDBFSTYLE.SEEK(lcStyle,'STYLE'),STYLE.Desc,'')
        lcStyPr = ALLTRIM(STR(STYLE.nSugRetPri,6,2))
        loDBFSCALE.SEEK('S'+STYLE.Scale,'SCALE')
        FOR I=1 TO SCALE.Cnt
          lcSz = STR(I,1)
          IF Posln.Qty&lcSz > 0
            lcZDesc = SCALE.SZ&lcSz 
            lcStySz = lcStyle + lcSz
            *--Increase qty with process persentage.
            lnQty   = CEILING(POSLN.Qty&lcSz * (lnRPPerc/100))
            SELECT (lcTmpBars)
            FOR lnQtyCount = 1 TO lnQty
              
                IF lnLBCount =1
                  APPEND BLANK
                ENDIF
                DO CASE
                  CASE lnlbCount =1
                    REPLACE cCode    WITH lcStySz,;
                            cStyle   WITH lcStyCd+lcSz,; 
                            SDesc    WITH lcSDesc,;
                            SizeDesc WITH lcZDesc,;
                            cPrice   WITH IIF(llPrintPrc,lcStyPr,'') 
                  CASE lnlbCount =2
                    REPLACE cCode1    WITH lcStySz,;
                            cStyle1   WITH lcStyCd+lcSz,; 
                            SDesc1    WITH lcSDesc,;
                            SizeDesc1 WITH lcZDesc,;
                            cPrice1   WITH IIF(llPrintPrc,lcStyPr,'') 
                  CASE lnlbCount =3          
                    REPLACE cCode2    WITH lcStySz,;
                            cStyle2   WITH lcStyCd+lcSz,; 
                            SDesc2    WITH lcSDesc,;
                            SizeDesc2 WITH lcZDesc,;
                            cPrice2   WITH IIF(llPrintPrc,lcStyPr,'') 

                ENDCASE
                lnlbCount = lnlbCount  + 1
                IF lnlbCount = 4
                  lnlbCount = 1
                ENDIF
              ENDFOR
            ENDIF
          ENDFOR  
      ENDSCAN 
    ENDIF                   
    ELSE

      IF loDBFPOSLN.Sqlrun("Select POSLN.CSTYTYPE,POSLN.CBUSDOCU ,POSLN.style,POSLN.PO,"+;
                           "posln.qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty,trancd,Posln.cinvtype,posln.[lineno]"+;
                           "FROM POSLN(INDEX = POSLN) INNER JOIN POSHDR(INDEX = POSHDR) "+;
                           " ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND POSLN.CSTYTYPE =POSHDR.CSTYTYPE AND "+;
                           " POSLN.PO = POSHDR.PO WHERE POSLN.CBUSDOCU = '"+IIF(lcPoTyp = "M",'P',IIF(lcPoTyp = "P",'P','N'))+;
                           "' AND POSLN.CSTYTYPE = '"+IIF(lcPoTyp = "M",'U',IIF(lcPoTyp = "P",'P','N'))+"' AND Poshdr.Status <> 'X' AND "+;
                           IIF(lcRpBasdOn = 'R'," TranCd = '2' AND POSHDR.Receive > 0"," TranCd = '1'"))

        SELECT Posln 
        WAIT WINDOW 'Collecting Data.......' nOWAIT 
        SCAN

*        WAIT WINDOW 'P/O#:'+ Posln.PO +'  Style:'+ Posln.Style  nOWAIT 
        lcStyle = PADR(POSLN.Style,lnStyLen)
        lcStyCd = lfRemSepr(lcStyle)
        lcSDesc = IIF(loDBFSTYLE.SEEK(lcStyle,'STYLE'),STYLE.Desc,'')
        lcStyPr = ALLTRIM(STR(STYLE.nSugRetPri,6,2))
        loDBFSCALE.SEEK('S'+STYLE.Scale,'SCALE')
        
        FOR I=1 TO SCALE.CNT
          lcSz = STR(I,1)
          IF PosLn.Qty&lcSz > 0
            lcZDesc = SCALE.SZ&lcSz 
            lcStySz = lcStyle + lcSz
            *--Increase qty with process persentage.
            lnQty   = CEILING(POSLN.Qty&lcSz * (lnRPPerc/100))
            SELECT (lcTmpBars)
            FOR lnQtyCount = 1 TO lnQty
    
              IF lnLBCount =1
                APPEND BLANK
              ENDIF
              DO CASE
                CASE lnlbCount =1
                  REPLACE cCode    WITH lcStySz,;
                          cStyle   WITH lcStyCd+lcSz,; 
                          SDesc    WITH lcSDesc,;
                          SizeDesc WITH lcZDesc,;
                          cPrice   WITH IIF(llPrintPrc,lcStyPr,'') 
                CASE lnlbCount =2
                  REPLACE cCode1    WITH lcStySz,;
                          cStyle1   WITH lcStyCd+lcSz,; 
                          SDesc1    WITH lcSDesc,;
                          SizeDesc1 WITH lcZDesc,;
                          cPrice1   WITH IIF(llPrintPrc,lcStyPr,'') 
                CASE lnlbCount =3          
                  REPLACE cCode2    WITH lcStySz,;
                          cStyle2   WITH lcStyCd+lcSz,; 
                          SDesc2    WITH lcSDesc,;
                          SizeDesc2 WITH lcZDesc,;
                          cPrice2   WITH IIF(llPrintPrc,lcStyPr,'') 

              ENDCASE
              lnlbCount = lnlbCount  + 1
              IF lnlbCount = 4
                lnlbCount = 1
              ENDIF
      
            ENDFOR
          ENDIF
        ENDFOR  

      ENDSCAN 
*      WAIT WINDOW SECONDS()  - lcTim 
    ENDIF                   
    ENDIF  
  ENDIF 
ENDIF  
   
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/13/2006
*! Purpose   : Function to create temp.  file 
*!*************************************************************
FUNCTION lfCreateTemp

DIMENSION laStruArr[15, 4]

laStruArr[1  ,1] = 'cCode'
laStruArr[1  ,2] = 'C'
laStruArr[1  ,3] = 20
laStruArr[1  ,4] = 0

laStruArr[2  ,1] = 'cStyle'
laStruArr[2  ,2] = 'C'
laStruArr[2  ,3] = 20
laStruArr[2  ,4] = 0

laStruArr[3  ,1] = 'SDesc'
laStruArr[3  ,2] = 'C'
laStruArr[3  ,3] = 23
laStruArr[3  ,4] = 0

laStruArr[4  ,1] = 'SizeDesc'
laStruArr[4  ,2] = 'C'
laStruArr[4  ,3] = 5
laStruArr[4  ,4] = 0


laStruArr[5  ,1] = 'cPrice'
laStruArr[5  ,2] = 'C'
laStruArr[5  ,3] = 6
laStruArr[5  ,4] = 0

laStruArr[6  ,1] = 'cCode1'
laStruArr[6  ,2] = 'C'
laStruArr[6  ,3] = 20
laStruArr[6  ,4] = 0

laStruArr[7  ,1] = 'cStyle1'
laStruArr[7  ,2] = 'C'
laStruArr[7 ,3] = 20
laStruArr[7  ,4] = 0

laStruArr[8  ,1] = 'SDesc1'
laStruArr[8  ,2] = 'C'
laStruArr[8  ,3] = 23
laStruArr[8  ,4] = 0

laStruArr[9  ,1] = 'SizeDesc1'
laStruArr[9  ,2] = 'C'
laStruArr[9  ,3] = 5
laStruArr[9  ,4] = 0

laStruArr[10  ,1] = 'cPrice1'
laStruArr[10  ,2] = 'C'
laStruArr[10  ,3] = 6
laStruArr[10  ,4] = 0

laStruArr[11  ,1] = 'cCode2'
laStruArr[11  ,2] = 'C'
laStruArr[11  ,3] = 20
laStruArr[11  ,4] = 0

laStruArr[12  ,1] = 'cStyle2'
laStruArr[12  ,2] = 'C'
laStruArr[12  ,3] = 20
laStruArr[12  ,4] = 0

laStruArr[13  ,1] = 'SDesc2'
laStruArr[13  ,2] = 'C'
laStruArr[13  ,3] = 23
laStruArr[13  ,4] = 0

laStruArr[14  ,1] = 'SizeDesc2'
laStruArr[14  ,2] = 'C'
laStruArr[14  ,3] = 5
laStruArr[14  ,4] = 0

laStruArr[15  ,1] = 'cPrice2'
laStruArr[15  ,2] = 'C'
laStruArr[15  ,3] = 6
laStruArr[15  ,4] = 0

=gfCrtTmp(lcTmpBars,@laStruArr,.F.,lcTmpBars,.T.)


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

