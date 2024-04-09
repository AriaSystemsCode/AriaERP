This file is to help you to install the CRM module on your server.
Software requirements.

1 – Aria 2.7.
2 – IIS 4 or later.
3 – Crystal report 8 or later.
4 – MS SQL server 7 or higher.
 

Setup instruction for the CRM:

1 – Run the self extracting zip file and unzip it in your WWWROOT folder i.e. (“C:\INETPUB\WWWROOT\”) it’ll create a new folder there called “CRM”.
2 – Go to the files extracted and open the folder called “Setup”.
3 – Run the setup.exe and follow the instructions.
4 – When the program asking you about the path, type the CRM path that you just created by the zip file in step 1.
5 – Create in your SQL server two databases called (“TRANSLOG” and “WEBPRODUCTS”)
6 – Open the “SQL Query Analyzer” and connect to your SQL server and chose “TRANSLOG” database.
7 – Open the file called “TRANSLOG.SQL” located under the CRM directory.
8 – Run the script from the “TRANSLOG.SQL” to create the tables in TRANSLOG database.
9 – Repeat steps 6, 7, and 8 by connecting to your SQL server on database “WEBPRODUCTS”.
10 – Open the file called “WEBPRODUCTS.SQL” located under the CRM directory.
11 - Run the script from the “WEBPRODUCTS.SQL” to create the tables in WEBPRODUCTS database.
12 – Open the IIS to configure your CRM web directory.
13 – From the IIS tree view, chose the CRM directory and right click on it.
14 – Select propriety.
15 – Click on Create directory.
16 – Now you need to setup the security of the directories on your server.
17 – Make the directory “ADMIN” under the CRM directory has NT authentication and disabled the anonymous access, so no one can open this directory except the specified users.
18 – Give the internet user “Read/Write” access to your Aria 2.7 data and “SYSFILES”.
19 – Read the included manual* to be able to continue the setup of the CRM module on your system.


* The CRM manual will be located in the directory of the CRM created from step 1.

 

For more information please contact Aria Systems. or visit our web site http://www.ariany.com