*:***************************************************************************
*: Program file  : ARLETER
*: Program desc. : Customer Letters
*:        System : Aria Advantage Series.
*:        Module : Accounts Receiviable (AR)
*:        Date   : 10/30/1999
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Example : DO ARLETER -- E301119,1 Customer/Vendor Letters
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*
DIMENSION laLetrType[4]
laLetrType[1] = "Extension"
laLetrType[2] = "Cancellation"
laLetrType[3] = "Decline"
laLetrType[4] = "General Purpose"

DO (gcAppHome+"ARPLETER.FXP") WITH "C"
*-- end of program code.
