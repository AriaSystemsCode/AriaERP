*:***************************************************************************
*: Program file  : APLETER
*: Program desc. : Vendor Letters
*:        System : Aria Advantage Series.
*:        Module : Accounts Payable (AP)
*:        Date   : 10/30/1999
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Example : DO ARLETER -- E301119,1 Customer/Vendor Letters
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*
DIMENSION laLetrType[1]
laLetrType[1] = "General Purpose"

DO (gcAppHome+"ARPLETER.FXP") WITH "V"
*-- end of program code.
