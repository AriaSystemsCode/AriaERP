using System;
using System.Collections.Generic;
using System.Text;
using Aria.DataTypes.ObjectDictionary;
using System.Collections;
using Aria.DataTypes;
using Aria.DataTypes.Settings;
using System.EnterpriseServices;

namespace Aria.EnterpriseServices.ObjectDictionary
{
    public class AriaObjectDictionaryManager : ServicedComponent 
    {
        public ArrayList GetAriaObjectTypes()
        {
            ArrayList objectTypesNamesList = new ArrayList(Enum.GetNames(typeof(AriaObjectTypes)));
            
            return objectTypesNamesList;
        }


        public ArrayList GetDataPaths()
        {
            ArrayList arrayList = new ArrayList();

            AriaDataPath dataPath1 = new AriaDataPath();

            dataPath1.DataPath = "ProjectType";
            dataPath1.DataType = AriaDataTypes.Field;
            dataPath1.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath1.Settings).FieldName = "ProjectType";
            ((AriaFieldSettings)dataPath1.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath1.Settings).Width = 1;
            ((AriaFieldSettings)dataPath1.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath1.Settings).Code = "";
            ((AriaFieldSettings)dataPath1.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath1.Settings).ValidEntry = false;
            arrayList.Add(dataPath1);


            AriaDataPath dataPath2 = new AriaDataPath();
            dataPath2.DataPath = "ProjectID";
            dataPath2.DataType = AriaDataTypes.Field;
            dataPath2.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath2.Settings).FieldName = "ProjectID";
            ((AriaFieldSettings)dataPath2.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath2.Settings).Width = 6;
            ((AriaFieldSettings)dataPath2.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath2.Settings).Code = "";
            ((AriaFieldSettings)dataPath2.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath2.Settings).ValidEntry = false;
            arrayList.Add(dataPath2);

            AriaDataPath dataPath3 = new AriaDataPath();
            dataPath3.DataPath = "StyleMajor";
            dataPath3.DataType = AriaDataTypes.Field;
            dataPath3.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath3.Settings).FieldName = "StyleMajor";
            ((AriaFieldSettings)dataPath3.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath3.Settings).Width = 12;
            ((AriaFieldSettings)dataPath3.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath3.Settings).Code = "";
            ((AriaFieldSettings)dataPath3.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath3.Settings).ValidEntry = false;
            arrayList.Add(dataPath3);

            AriaDataPath dataPath4 = new AriaDataPath();
            dataPath4.DataPath = "CategorySequence";
            dataPath4.DataType = AriaDataTypes.Field;
            dataPath4.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath4.Settings).FieldName = "CategorySequence";
            ((AriaFieldSettings)dataPath4.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath4.Settings).Width = 2;
            ((AriaFieldSettings)dataPath4.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath4.Settings).Code = "";
            ((AriaFieldSettings)dataPath4.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath4.Settings).ValidEntry = false;
            arrayList.Add(dataPath4);

            AriaDataPath dataPath5 = new AriaDataPath();
            dataPath5.DataPath = "OperationCategory";
            dataPath5.DataType = AriaDataTypes.Field;
            dataPath5.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath5.Settings).FieldName = "OperationCategory";
            ((AriaFieldSettings)dataPath5.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath5.Settings).Width = 3;
            ((AriaFieldSettings)dataPath5.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath5.Settings).Code = "";
            ((AriaFieldSettings)dataPath5.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath5.Settings).ValidEntry = false;
            arrayList.Add(dataPath5);

            AriaDataPath dataPath6 = new AriaDataPath();
            dataPath6.DataPath = "OperationSquence";
            dataPath6.DataType = AriaDataTypes.Field;
            dataPath6.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath6.Settings).FieldName = "OperationSquence";
            ((AriaFieldSettings)dataPath6.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath6.Settings).Width = 2;
            ((AriaFieldSettings)dataPath6.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath6.Settings).Code = "";
            ((AriaFieldSettings)dataPath6.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath6.Settings).ValidEntry = false;
            arrayList.Add(dataPath6);

            AriaDataPath dataPath7 = new AriaDataPath();
            dataPath7.DataPath = "OperationID";
            dataPath7.DataType = AriaDataTypes.Field;
            dataPath7.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath7.Settings).FieldName = "OperationID";
            ((AriaFieldSettings)dataPath7.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath7.Settings).Width = 5;
            ((AriaFieldSettings)dataPath7.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath7.Settings).Code = "";
            ((AriaFieldSettings)dataPath7.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath7.Settings).ValidEntry = false;
            arrayList.Add(dataPath7);

            AriaDataPath dataPath8 = new AriaDataPath();
            dataPath8.DataPath = "CalculatedStart";
            dataPath8.DataType = AriaDataTypes.Field;
            dataPath8.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath8.Settings).FieldName = "CalculatedStart";
            ((AriaFieldSettings)dataPath8.Settings).DataType = AriaStandardDataTypes.Date;
            ((AriaFieldSettings)dataPath8.Settings).Width = 8;
            ((AriaFieldSettings)dataPath8.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath8.Settings).Code = "";
            ((AriaFieldSettings)dataPath8.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath8.Settings).ValidEntry = false;
            arrayList.Add(dataPath8);

            AriaDataPath dataPath9 = new AriaDataPath();
            dataPath9.DataPath = "CalculatedFinish";
            dataPath9.DataType = AriaDataTypes.Field;
            dataPath9.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath9.Settings).FieldName = "CalculatedFinish";
            ((AriaFieldSettings)dataPath9.Settings).DataType = AriaStandardDataTypes.Date;
            ((AriaFieldSettings)dataPath9.Settings).Width = 8;
            ((AriaFieldSettings)dataPath9.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath9.Settings).Code = "";
            ((AriaFieldSettings)dataPath9.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath9.Settings).ValidEntry = false;
            arrayList.Add(dataPath9);
            
            AriaDataPath dataChildPath = new AriaDataPath();
            dataChildPath.DataPath = "ProjectNotification";
            dataChildPath.DataType = AriaDataTypes.AriaDictionaryDefinedObject;
            dataChildPath.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath.Settings).FieldName = "ProjectNotification";
            arrayList.Add(dataChildPath);

            AriaDataPath dataChildPath1 = new AriaDataPath();
            dataChildPath1.DataPath = "ProjectNotification.UserID";
            dataChildPath1.DataType = AriaDataTypes.Field;
            dataChildPath1.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath1.Settings).FieldName = "UserID";
            ((AriaFieldSettings)dataChildPath1.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath1.Settings).Width = 10;
            ((AriaFieldSettings)dataChildPath1.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath1.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath1.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath1.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath1);

            AriaDataPath dataChildPath2 = new AriaDataPath();
            dataChildPath2.DataPath = "ProjectNotification.UserEmail";
            dataChildPath2.DataType = AriaDataTypes.Field;
            dataChildPath2.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath2.Settings).FieldName = "UserEmail";
            ((AriaFieldSettings)dataChildPath2.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath2.Settings).Width = 60;
            ((AriaFieldSettings)dataChildPath2.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath2.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath2.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath2.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath2);

            AriaDataPath dataChildPath3 = new AriaDataPath();
            dataChildPath3.DataPath = "ProjectNotification.NotifyOnStart";
            dataChildPath3.DataType = AriaDataTypes.Field;
            dataChildPath3.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath3.Settings).FieldName = "NotifyOnStart";
            ((AriaFieldSettings)dataChildPath3.Settings).DataType = AriaStandardDataTypes.Logical;
            ((AriaFieldSettings)dataChildPath3.Settings).Width = 0;
            ((AriaFieldSettings)dataChildPath3.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath3.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath3.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath3.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath3);

            AriaDataPath dataChildPath4 = new AriaDataPath();
            dataChildPath4.DataPath = "ProjectNotification.NotifyOnComplete";
            dataChildPath4.DataType = AriaDataTypes.Field;
            dataChildPath4.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath4.Settings).FieldName = "NotifyOnComplete";
            ((AriaFieldSettings)dataChildPath4.Settings).DataType = AriaStandardDataTypes.Logical;
            ((AriaFieldSettings)dataChildPath4.Settings).Width = 0;
            ((AriaFieldSettings)dataChildPath4.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath4.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath4.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath4.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath4);

            AriaDataPath dataChildPath5 = new AriaDataPath();
            dataChildPath5.DataPath = "ProjectNotification.NotifyBeforeStart";
            dataChildPath5.DataType = AriaDataTypes.Field;
            dataChildPath5.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath5.Settings).FieldName = "NotifyBeforeStart";
            ((AriaFieldSettings)dataChildPath5.Settings).DataType = AriaStandardDataTypes.Logical;
            ((AriaFieldSettings)dataChildPath5.Settings).Width = 0;
            ((AriaFieldSettings)dataChildPath5.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath5.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath5.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath5.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath5);

            AriaDataPath dataChildPath6 = new AriaDataPath();
            dataChildPath6.DataPath = "ProjectNotification.NotifyBeforeStartDays";
            dataChildPath6.DataType = AriaDataTypes.Field;
            dataChildPath6.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath6.Settings).FieldName = "NotifyBeforeStartDays";
            ((AriaFieldSettings)dataChildPath6.Settings).DataType = AriaStandardDataTypes.Numeric;
            ((AriaFieldSettings)dataChildPath6.Settings).Width = 3;
            ((AriaFieldSettings)dataChildPath6.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath6.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath6.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath6.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath6);

            AriaDataPath dataChildPath7 = new AriaDataPath();
            dataChildPath7.DataPath = "ProjectNotification.NotifyBeforeComplete";
            dataChildPath7.DataType = AriaDataTypes.Field;
            dataChildPath7.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath7.Settings).FieldName = "NotifyBeforeComplete";
            ((AriaFieldSettings)dataChildPath7.Settings).DataType = AriaStandardDataTypes.Logical;
            ((AriaFieldSettings)dataChildPath7.Settings).Width = 0;
            ((AriaFieldSettings)dataChildPath7.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath7.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath7.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath7.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath7);

            AriaDataPath dataChildPath8 = new AriaDataPath();
            dataChildPath8.DataPath = "ProjectNotification.NotifyBeforeCompleteDays";
            dataChildPath8.DataType = AriaDataTypes.Field;
            dataChildPath8.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath8.Settings).FieldName = "NotifyBeforeCompleteDays";
            ((AriaFieldSettings)dataChildPath8.Settings).DataType = AriaStandardDataTypes.Numeric;
            ((AriaFieldSettings)dataChildPath8.Settings).Width = 3;
            ((AriaFieldSettings)dataChildPath8.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath8.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath8.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath8.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath8);

            AriaDataPath dataChildPath9 = new AriaDataPath();
            dataChildPath9.DataPath = "ProjectNotification.NotifyBeforeStartDate";
            dataChildPath9.DataType = AriaDataTypes.Field;
            dataChildPath9.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath9.Settings).FieldName = "NotifyBeforeStartDate";
            ((AriaFieldSettings)dataChildPath9.Settings).DataType = AriaStandardDataTypes.Date;
            ((AriaFieldSettings)dataChildPath9.Settings).Width = 8;
            ((AriaFieldSettings)dataChildPath9.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath9.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath9.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath9.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath9);

            AriaDataPath dataChildPath10 = new AriaDataPath();
            dataChildPath10.DataPath = "ProjectNotification.NotifyBeforeCompleteDate";
            dataChildPath10.DataType = AriaDataTypes.Field;
            dataChildPath10.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath10.Settings).FieldName = "NotifyBeforeCompleteDate";
            ((AriaFieldSettings)dataChildPath10.Settings).DataType = AriaStandardDataTypes.Date;
            ((AriaFieldSettings)dataChildPath10.Settings).Width = 8;
            ((AriaFieldSettings)dataChildPath10.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath10.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath10.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath10.Settings).ValidEntry = false;
            dataChildPath.ChildDataPaths.Add(dataChildPath10);

            return arrayList;
        }


        public ArrayList GetDataPaths1()
        {
            ArrayList arrayList = new ArrayList();

            //AriaDataPath dataPath1 = new AriaDataPath();
            //dataPath1.DataPath = "SalesOrderType";
            //dataPath1.DataType = AriaDataTypes.Field;
            //dataPath1.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath1.Settings).FieldName = "SalesOrderType";
            //((AriaFieldSettings)dataPath1.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath1.Settings).Width = 1;
            //((AriaFieldSettings)dataPath1.Settings).DecimalPlaces  = 0;
            //((AriaFieldSettings)dataPath1.Settings).Code = "";
            //((AriaFieldSettings)dataPath1.Settings).ValidEntries = new string[] {"O|Open", "H|Hold", "X|Cancel"};
            //((AriaFieldSettings)dataPath1.Settings).ValidEntry  = true;
            //arrayList.Add(dataPath1);

            AriaDataPath dataPath2 = new AriaDataPath();
            dataPath2.DataPath = "SalesOrderNumber";
            dataPath2.DataType = AriaDataTypes.Field;
            dataPath2.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath2.Settings).FieldName = "SalesOrderNumber";
            ((AriaFieldSettings)dataPath2.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath2.Settings).Width = 6;
            ((AriaFieldSettings)dataPath2.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath2.Settings).Code = "";
            ((AriaFieldSettings)dataPath2.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath2.Settings).ValidEntry = false;
            arrayList.Add(dataPath2);

            AriaDataPath dataPath3 = new AriaDataPath();
            dataPath3.DataPath = "EDIOrder";
            dataPath3.DataType = AriaDataTypes.Field;
            dataPath3.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath3.Settings).FieldName = "EDIOrder";
            ((AriaFieldSettings)dataPath3.Settings).DataType = AriaStandardDataTypes.Logical;
            ((AriaFieldSettings)dataPath3.Settings).Width = 1;
            ((AriaFieldSettings)dataPath3.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath3.Settings).Code = "";
            ((AriaFieldSettings)dataPath3.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath3.Settings).ValidEntry = false;
            arrayList.Add(dataPath3);

            AriaDataPath dataPath4 = new AriaDataPath();
            dataPath4.DataPath = "EnteredDate";
            dataPath4.DataType = AriaDataTypes.Field;
            dataPath4.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath4.Settings).FieldName = "EnteredDate";
            ((AriaFieldSettings)dataPath4.Settings).DataType = AriaStandardDataTypes.Date;
            ((AriaFieldSettings)dataPath4.Settings).Width = 8;
            ((AriaFieldSettings)dataPath4.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath4.Settings).Code = "";
            ((AriaFieldSettings)dataPath4.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath4.Settings).ValidEntry = false;
            arrayList.Add(dataPath4);

            //System.Windows.Forms.MessageBox.Show("1");
            AriaDataPath dataPath41 = new AriaDataPath();
            dataPath41.DataPath = "CompleteDate";
            dataPath41.DataType = AriaDataTypes.Field;
            dataPath41.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath41.Settings).FieldName = "CompleteDate";
            ((AriaFieldSettings)dataPath41.Settings).DataType = AriaStandardDataTypes.Date;
            ((AriaFieldSettings)dataPath41.Settings).Width = 8;
            ((AriaFieldSettings)dataPath41.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath41.Settings).Code = "";
            ((AriaFieldSettings)dataPath41.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath41.Settings).ValidEntry = false;
            arrayList.Add(dataPath41);

            
            AriaDataPath dataPath5 = new AriaDataPath();
            dataPath5.DataPath = "Status";
            dataPath5.DataType = AriaDataTypes.Field;
            dataPath5.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath5.Settings).FieldName = "Status";
            ((AriaFieldSettings)dataPath5.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath5.Settings).Width = 1;
            ((AriaFieldSettings)dataPath5.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath5.Settings).Code = "";
            ((AriaFieldSettings)dataPath5.Settings).ValidEntries = new string[] { "B|Bid", "H|Hold", "O|Open", "X|Cancle", "C|Complete"};
            ((AriaFieldSettings)dataPath5.Settings).ValidEntry = true;
            arrayList.Add(dataPath5);

            AriaDataPath dataPath6 = new AriaDataPath();
            dataPath6.DataPath = "CustomerCode";
            dataPath6.DataType = AriaDataTypes.Field;
            dataPath6.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath6.Settings).FieldName = "CustomerCode";
            ((AriaFieldSettings)dataPath6.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath6.Settings).Width = 5;
            ((AriaFieldSettings)dataPath6.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath6.Settings).Code = "";
            ((AriaFieldSettings)dataPath6.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath6.Settings).ValidEntry = false;
            arrayList.Add(dataPath6);

            AriaDataPath dataPath7 = new AriaDataPath();
            dataPath7.DataPath = "StoreNumber";
            dataPath7.DataType = AriaDataTypes.Field;
            dataPath7.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath7.Settings).FieldName = "StoreNumber";
            ((AriaFieldSettings)dataPath7.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath7.Settings).Width = 8;
            ((AriaFieldSettings)dataPath7.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath7.Settings).Code = "";
            ((AriaFieldSettings)dataPath7.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath7.Settings).ValidEntry = false;
            arrayList.Add(dataPath7);

            AriaDataPath dataPath8 = new AriaDataPath();
            dataPath8.DataPath = "Division";
            dataPath8.DataType = AriaDataTypes.Field;
            dataPath8.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath8.Settings).FieldName = "Division";
            ((AriaFieldSettings)dataPath8.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath8.Settings).Width = 6;
            ((AriaFieldSettings)dataPath8.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath8.Settings).Code = "CDIVISION";
            ((AriaFieldSettings)dataPath8.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath8.Settings).ValidEntry = false;
            arrayList.Add(dataPath8);

            AriaDataPath dataPath9 = new AriaDataPath();
            dataPath9.DataPath = "SalesRep1";
            dataPath9.DataType = AriaDataTypes.Field;
            dataPath9.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath9.Settings).FieldName = "SalesRep1";
            ((AriaFieldSettings)dataPath9.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataPath9.Settings).Width = 3;
            ((AriaFieldSettings)dataPath9.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataPath9.Settings).Code = "";
            ((AriaFieldSettings)dataPath9.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath9.Settings).ValidEntry = false;
            arrayList.Add(dataPath9);

            AriaDataPath dataPath10 = new AriaDataPath();
            dataPath10.DataPath = "OpenAmount";
            dataPath10.DataType = AriaDataTypes.Field;
            dataPath10.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataPath10.Settings).FieldName = "OpenAmount";
            ((AriaFieldSettings)dataPath10.Settings).DataType = AriaStandardDataTypes.Numeric;
            ((AriaFieldSettings)dataPath10.Settings).Width = 14;
            ((AriaFieldSettings)dataPath10.Settings).DecimalPlaces = 2;
            ((AriaFieldSettings)dataPath10.Settings).Code = "";
            ((AriaFieldSettings)dataPath10.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataPath10.Settings).ValidEntry = false;
            arrayList.Add(dataPath10);


            /////////////////////////////////////////////////////////////

            AriaDataPath dataChildPathCustomer = new AriaDataPath();
            dataChildPathCustomer.DataPath = "Customer";
            dataChildPathCustomer.DataType = AriaDataTypes.AriaDictionaryDefinedObject;
            dataChildPathCustomer.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPathCustomer.Settings).FieldName = "Customer";
            arrayList.Add(dataChildPathCustomer);

            AriaDataPath dataChildPath1 = new AriaDataPath();
            dataChildPath1.DataPath = "Customer.CustomerCode";
            dataChildPath1.DataType = AriaDataTypes.Field;
            dataChildPath1.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath1.Settings).FieldName = "CustomerCode";
            ((AriaFieldSettings)dataChildPath1.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath1.Settings).Width = 5;
            ((AriaFieldSettings)dataChildPath1.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath1.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath1.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath1.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath1);

            AriaDataPath dataChildPath2 = new AriaDataPath();
            dataChildPath2.DataPath = "Customer.StoreNumber";
            dataChildPath2.DataType = AriaDataTypes.Field;
            dataChildPath2.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath2.Settings).FieldName = "StoreNumber";
            ((AriaFieldSettings)dataChildPath2.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath2.Settings).Width = 8;
            ((AriaFieldSettings)dataChildPath2.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath2.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath2.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath2.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath2);

            AriaDataPath dataChildPath3 = new AriaDataPath();
            dataChildPath3.DataPath = "Customer.CustomerStatus";
            dataChildPath3.DataType = AriaDataTypes.Field;
            dataChildPath3.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath3.Settings).FieldName = "CustomerStatus";
            ((AriaFieldSettings)dataChildPath3.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath3.Settings).Width = 1;
            ((AriaFieldSettings)dataChildPath3.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath3.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath3.Settings).ValidEntries = new string[] { "A|Active", "H|Hold", "X|Cancel"};
            ((AriaFieldSettings)dataChildPath3.Settings).ValidEntry = true;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath3);

            AriaDataPath dataChildPath4 = new AriaDataPath();
            dataChildPath4.DataPath = "Customer.BillToName";
            dataChildPath4.DataType = AriaDataTypes.Field;
            dataChildPath4.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath4.Settings).FieldName = "BillToName";
            ((AriaFieldSettings)dataChildPath4.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath4.Settings).Width = 30;
            ((AriaFieldSettings)dataChildPath4.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath4.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath4.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath4.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath4);

            AriaDataPath dataChildPath5 = new AriaDataPath();
            dataChildPath5.DataPath = "Customer.ShipToName";
            dataChildPath5.DataType = AriaDataTypes.Field;
            dataChildPath5.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath5.Settings).FieldName = "ShipToName";
            ((AriaFieldSettings)dataChildPath5.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath5.Settings).Width = 30;
            ((AriaFieldSettings)dataChildPath5.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath5.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath5.Settings).ValidEntries = new string[] {  };
            ((AriaFieldSettings)dataChildPath5.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath5);

            AriaDataPath dataChildPath6 = new AriaDataPath();
            dataChildPath6.DataPath = "Customer.CreditLimit";
            dataChildPath6.DataType = AriaDataTypes.Field;
            dataChildPath6.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath6.Settings).FieldName = "CreditLimit";
            ((AriaFieldSettings)dataChildPath6.Settings).DataType = AriaStandardDataTypes.Numeric;
            ((AriaFieldSettings)dataChildPath6.Settings).Width = 11;
            ((AriaFieldSettings)dataChildPath6.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath6.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath6.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath6.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath6);

            AriaDataPath dataChildPath7 = new AriaDataPath();
            dataChildPath7.DataPath = "Customer.NetBalance";
            dataChildPath7.DataType = AriaDataTypes.Field;
            dataChildPath7.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath7.Settings).FieldName = "NetBalance";
            ((AriaFieldSettings)dataChildPath7.Settings).DataType = AriaStandardDataTypes.Numeric;
            ((AriaFieldSettings)dataChildPath7.Settings).Width = 14;
            ((AriaFieldSettings)dataChildPath7.Settings).DecimalPlaces = 2;
            ((AriaFieldSettings)dataChildPath7.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath7.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath7.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath7);

            AriaDataPath dataChildPath8 = new AriaDataPath();
            dataChildPath8.DataPath = "Customer.EmailAddress";
            dataChildPath8.DataType = AriaDataTypes.Field;
            dataChildPath8.Settings = new AriaFieldSettings();
            ((AriaFieldSettings)dataChildPath8.Settings).FieldName = "EmailAddress";
            ((AriaFieldSettings)dataChildPath8.Settings).DataType = AriaStandardDataTypes.String;
            ((AriaFieldSettings)dataChildPath8.Settings).Width = 60;
            ((AriaFieldSettings)dataChildPath8.Settings).DecimalPlaces = 0;
            ((AriaFieldSettings)dataChildPath8.Settings).Code = "";
            ((AriaFieldSettings)dataChildPath8.Settings).ValidEntries = new string[] { };
            ((AriaFieldSettings)dataChildPath8.Settings).ValidEntry = false;
            dataChildPathCustomer.ChildDataPaths.Add(dataChildPath8);
            
            //AriaDataPath dataPath2 = new AriaDataPath();
            //dataPath2.DataPath = "SalesOrderNumber";
            //dataPath2.DataType = AriaDataTypes.Field;
            //dataPath2.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath2.Settings).FieldName = "SalesOrderNumber";
            //((AriaFieldSettings)dataPath2.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath2.Settings).Width = 6;
            //((AriaFieldSettings)dataPath2.Settings).DecimalPlaces = 0;
            //((AriaFieldSettings)dataPath2.Settings).Code = "";
            //((AriaFieldSettings)dataPath2.Settings).ValidEntries = new string[] { };
            //((AriaFieldSettings)dataPath2.Settings).ValidEntry = false;
            //arrayList.Add(dataPath2);


            //return arrayList;
            //////////////////////////                        
            //AriaDataPath dataPath11 = new AriaDataPath();
            //dataPath11.DataPath = "1.1";
            //dataPath11.DataType = AriaDataTypes.Field;
            //dataPath11.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath11.Settings).DataType = AriaStandardDataTypes.Logical;
            //((AriaFieldSettings)dataPath11.Settings).FieldName = "Field 1.1";
            //dataPath1.ChildDataPaths.Add(dataPath11);
            /////////////////////////
            //AriaDataPath dataPath12 = new AriaDataPath();
            //dataPath12.DataPath = "1.2";
            //dataPath12.DataType = AriaDataTypes.Field;
            //dataPath12.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath12.Settings).DataType = AriaStandardDataTypes.Numeric;
            //((AriaFieldSettings)dataPath12.Settings).FieldName = "Field 1.2";
            //dataPath1.ChildDataPaths.Add(dataPath12);
            /////////////////////////
            //AriaDataPath dataPath121 = new AriaDataPath();
            //dataPath121.DataPath = "1.2.1";
            //dataPath121.DataType = AriaDataTypes.Field;
            //dataPath121.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath121.Settings).DataType = AriaStandardDataTypes.Logical;
            //((AriaFieldSettings)dataPath121.Settings).FieldName = "Field 1.2.1";
            //dataPath12.ChildDataPaths.Add(dataPath121);
            /////////////////////////
            //AriaDataPath dataPath122 = new AriaDataPath();
            //dataPath122.DataPath = "1.2.2";
            //dataPath122.DataType = AriaDataTypes.Field;
            //dataPath122.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath122.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath122.Settings).FieldName = "Field 1.2.2";
            //dataPath12.ChildDataPaths.Add(dataPath122);
            /////////////////////////
            //AriaDataPath dataPath13 = new AriaDataPath();
            //dataPath13.DataPath = "1.3";
            //dataPath13.DataType = AriaDataTypes.Field;
            //dataPath13.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath13.Settings).DataType = AriaStandardDataTypes.Numeric;
            //((AriaFieldSettings)dataPath13.Settings).FieldName = "Field 1.3";
            //dataPath1.ChildDataPaths.Add(dataPath13);
            


            ///*********************************************************/



            //AriaDataPath dataPath2 = new AriaDataPath();
            //dataPath2.DataPath = "2";
            //dataPath2.DataType = AriaDataTypes.Field;
            //dataPath2.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath2.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath2.Settings).FieldName = "Field 2";
            //arrayList.Add(dataPath2);
            ///////////////////////////
            //AriaDataPath dataPath21 = new AriaDataPath();
            //dataPath21.DataPath = "2.1";
            //dataPath21.DataType = AriaDataTypes.Field;
            //dataPath21.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath21.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath21.Settings).FieldName = "Field 2.1";
            //dataPath2.ChildDataPaths.Add(dataPath21);
            ///////////////////////////
            //AriaDataPath dataPath22 = new AriaDataPath();
            //dataPath22.DataPath = "2.2";
            //dataPath22.DataType = AriaDataTypes.Field;
            //dataPath22.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath22.Settings).DataType = AriaStandardDataTypes.Date;
            //((AriaFieldSettings)dataPath22.Settings).FieldName = "Field 2.2";
            //dataPath2.ChildDataPaths.Add(dataPath22);



            
            ///*********************************************************/


            
            //AriaDataPath dataPath3 = new AriaDataPath();
            //dataPath3.DataPath = "3";
            //dataPath3.DataType = AriaDataTypes.Field;
            //dataPath3.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath3.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath3.Settings).FieldName = "Field 3";            
            //arrayList.Add(dataPath3);




            ///*********************************************************/



            
            //AriaDataPath dataPath4 = new AriaDataPath();
            //dataPath4.DataPath = "4";
            //dataPath4.DataType = AriaDataTypes.Field;
            //dataPath4.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath4.Settings).DataType = AriaStandardDataTypes.Int;
            //((AriaFieldSettings)dataPath4.Settings).FieldName = "Field 4";
            //arrayList.Add(dataPath4);


            
            
            
            ///*********************************************************/


            
            
            //AriaDataPath dataPath5 = new AriaDataPath();
            //dataPath5.DataPath = "5";
            //dataPath5.DataType = AriaDataTypes.Field;
            //dataPath5.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath5.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath5.Settings).FieldName = "Field 5";
            //arrayList.Add(dataPath5);




            ///*********************************************************/


            
            
            //AriaDataPath dataPath6 = new AriaDataPath();
            //dataPath6.DataPath = "6";
            //dataPath6.DataType = AriaDataTypes.Field;
            //dataPath6.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath6.Settings).DataType = AriaStandardDataTypes.Date;
            //((AriaFieldSettings)dataPath6.Settings).FieldName = "Field 6";            
            //arrayList.Add(dataPath6);
            ////////////////////////////
            //AriaDataPath dataPath61 = new AriaDataPath();
            //dataPath61.DataPath = "6.1";
            //dataPath61.DataType = AriaDataTypes.Field;
            //dataPath61.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath61.Settings).DataType = AriaStandardDataTypes.String;
            //((AriaFieldSettings)dataPath61.Settings).FieldName = "Field 6.1";
            //dataPath6.ChildDataPaths.Add(dataPath61);
            ///////////////////////////
            //AriaDataPath dataPath62 = new AriaDataPath();
            //dataPath62.DataPath = "6.2";
            //dataPath62.DataType = AriaDataTypes.Field;
            //dataPath62.Settings = new AriaFieldSettings();
            //((AriaFieldSettings)dataPath62.Settings).DataType = AriaStandardDataTypes.Logical;
            //((AriaFieldSettings)dataPath62.Settings).FieldName = "Field 6.2";
            //dataPath6.ChildDataPaths.Add(dataPath62);





            return arrayList;
        }
    }
}
