*C102356 Hassan To Calculate Check Deigit  [Begin]
FUNCTION FUCCCHCK
LPARAMETER lcUccNo
*
LOCAL lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount

STORE 0 TO  lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

*!* Detect The Type Of The "lcUccNo"
IF TYPE('lcUccNo') = 'C'
  lnTop = LEN(lcUccNo)
ENDIF

FOR lnCount = 1 TO lnTop STEP 2
  *!* Sum The Digits in Odd  Order Together.
  lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
  *!* Sum The Digits in Even Order Together.
  lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
ENDFOR

*!* Get The First Digit Of The Number.
lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)

RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))
*C102356 Hassan To Calculate Check Deigit  [END]
