using Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using System.Collections.ObjectModel;
using System.Xml.Serialization;
using System.IO;
using System.Xml;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            AccountWebservice.AccountWebServiceSoapClient t = new AccountWebservice.AccountWebServiceSoapClient();

            var restult = t.GetAccountObject(Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"));
        }

        private void button2_Click(object sender, EventArgs e)
        {
            AccountWebservice.AccountWebServiceSoapClient t = new AccountWebservice.AccountWebServiceSoapClient();
          
           // var result = s.GetAccountObject(Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"));
            var result = t.GetAllDemoAccounts();

        }

        private void button3_Click(object sender, EventArgs e)
        {
            SynchronizationManager x = new SynchronizationManager();
            //  x.PullData("0x0000000000002684", "0x0000000000002689", Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"));

            x.GetDBVersion(Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"));
        }

        private void button4_Click(object sender, EventArgs e)
        {
            //Test PushData
            SynchronizationManager x = new SynchronizationManager();
            //   x.PushData(Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"), x.PullData("0x0000000000000904", "0x0000000000000910", Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1")), "DeviceSignatue");

            // ServiceReference2.AriaSynchronizationServiceSoapClient x = new ServiceReference2.AriaSynchronizationServiceSoapClient();
            //  var r = t.PullData("0x000000000000569E", "0x00000000000056A0", Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"));
            //       var r = t.PullData("0x0000000000004B03", "0x000000000000322A", Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1"));
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = new List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>();

            SyncDataObject syncObj = new SyncDataObject();
            syncObj.AccountOID = Guid.Parse("be77804b-3406-5c47-0180-5eabac9b482d");
            syncObj.EntityOID = Guid.Parse("39c2eba1-ac5e-46ae-b7d7-98fe9c81ee3d");


            syncObj.TableName = "Entity";
            //syncObj.EntityType = "News";
            syncObj.RecordStatus = "Edit";
            syncObj.Version = "";
            syncObj.Properties = new List<string>();
            syncObj.Properties.Add("Notes");
            syncObj.Properties.Add("Description");
            syncObj.Properties.Add("Id");
            syncObj.Properties.Add("Status");
            syncObj.Properties.Add("StatusId");
            syncObj.Properties.Add("Type");
            syncObj.Properties.Add("TypeId");
            syncObj.Properties.Add("Category");
            syncObj.Properties.Add("CategoryId");
            syncObj.Properties.Add("Classification");
            syncObj.Properties.Add("ClassificationId");
            syncObj.Properties.Add("ExtraData");
            syncObj.Properties.Add("AddDate");
            syncObj.Properties.Add("GUID");
            syncObj.Properties.Add("Account");
            syncObj.Properties.Add("TransactionTimeStamp");
            syncObj.Values = new List<object>();
            syncObj.Values.Add("A panko-crusted lobster tail & jumbo grilled shrimp glazed with Captain Morgan BBQ sauce");
            syncObj.Values.Add("");
            syncObj.Values.Add("aaaa");
            syncObj.Values.Add(Guid.Parse("486B9231-AFD4-4359-99D2-DE4F3DDAC46A"));
            syncObj.Values.Add("ACTIVE");
            syncObj.Values.Add(Guid.Parse("B30F3F16-1FE4-409F-AB0F-9F1E52B92727"));
            syncObj.Values.Add("NEWS");
            syncObj.Values.Add(Guid.Parse("E2EDEA83-CF04-4526-9A84-C948061698F0"));
            syncObj.Values.Add("Specials");
            syncObj.Values.Add(Guid.Parse("8F42A62C-5839-4097-B0F1-6868AE1CDB4C"));
            syncObj.Values.Add("FOOD");
            //syncObj.Values.Add("<DocumentElement><ExtraData><Name>Name</Name><Value>Lobster and Shrimp in Paradise</Value></ExtraData><ExtraData><Name>CookingMethod</Name><Value>Grilled</Value></ExtraData><ExtraData><Name>Course</Name><Value>Appetizer</Value></ExtraData><ExtraData><Name>Cuisine</Name><Value>american</Value></ExtraData><ExtraData><Name>Dietary</Name><Value>Seafood</Value></ExtraData><ExtraData><Name>MajorIngredient</Name><Value>Lobster</Value></ExtraData><ExtraData><Name>Family</Name><Value></Value></ExtraData><ExtraData><Name>Price</Name><Value>100</Value></ExtraData><ExtraData><Name>Ingredients</Name><Value>lobster-shrimp</Value></ExtraData><ExtraData><Name>IsSpecial</Name><Value>False</Value></ExtraData><ExtraData><Name>IsNewIteml</Name><Value>False</Value></ExtraData><ExtraData><Name>StartPeriod</Name><Value>00:00:00</Value></ExtraData><ExtraData><Name>EndPeriod</Name><Value>00:00:00</Value></ExtraData><ExtraData><Name>Carbohydrates</Name><Value>97</Value></ExtraData><ExtraData><Name>Protein</Name><Value>57</Value></ExtraData><ExtraData><Name>Sodium</Name><Value>2460</Value></ExtraData><ExtraData><Name>Calories</Name><Value>1010</Value></ExtraData><ExtraData><Name>TotalFat</Name><Value>43</Value></ExtraData><ExtraData><Name>SaturatedFat</Name><Value>10</Value></ExtraData><ExtraData><Name>TransFat</Name><Value>0</Value></ExtraData><ExtraData><Name>Allergens</Name><Value>0</Value></ExtraData><ExtraData><Name>Cholesterol</Name><Value>3290</Value></ExtraData><ExtraData><Name>Sugar</Name><Value>30</Value></ExtraData></DocumentElement>");
            syncObj.Values.Add("<DocumentElement><ExtraData><Name>Title</Name><Value>21 DAYS OF LA: DAY 3 – SUNNIES </Value></ExtraData><ExtraData><Name>Article</Name><Value> Keeping the extra bright SoCal sun out of our eyes is practically a full-time job in itself. But who are we kidding, we love our third Los Angeles staple (sunnies) way more than any of our other essentials. Just don’t tell!</Value></ExtraData></DocumentElement>");
            syncObj.Values.Add(Convert.ToDateTime("6/18/2015 12:00:00"));
            syncObj.Values.Add("39c2eba1-ac5e-46ae-b7d7-98fe9c81ee1a");
            syncObj.Values.Add(Guid.Parse("8F42A62C-5839-4097-B0F1-6868AE1CDB4C"));
            syncObj.Values.Add(null);
            //syncObj.Values.Add("0e34c334-4dc4-405d-885f-01edf251ddec");

            syncObjList.Add(syncObj);


            ////var restult = x.PushData(Guid.Parse("be77804b-3406-5c47-0180-5eabac9b482d"), t.PullData("0x0000000000004B03", "0x000000000000322A", Guid.Parse("3451BEB5-50E1-41CA-A929-404976162BC1")), "DeviceSignatue");
            var restult = x.PushData(Guid.Parse("be77804b-3406-5c47-0180-5eabac9b482d"), syncObjList, "");

            //var restult = x.GetMinFrontEndVersion();

            // t.PushData(Guid.Parse("be77804b-3406-5c47-0180-5eabac9b482d"), syncObjList, "");

        }

        private void button4_Click_1(object sender, EventArgs e)
        {
            SynchronizationManager x = new SynchronizationManager();
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = new List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>();

            SyncDataObject syncObj = new SyncDataObject();
            syncObj.AccountOID = Guid.Parse("be77804b-3406-5c47-0180-5eabac9b482d");
            syncObj.EntityOID = Guid.Parse("39c2eba1-ac5e-46ae-b7d7-98fe9c81ee6d");


            syncObj.TableName = "EntityCategory";
            //syncObj.EntityType = "News";
            syncObj.RecordStatus = "ADD";
            syncObj.Version = "";
            syncObj.Properties = new List<string>();
            syncObj.Properties.Add("CategoryId");
            syncObj.Properties.Add("Name");
            syncObj.Properties.Add("EntityType");
            syncObj.Properties.Add("ParentCategory");
            syncObj.Values = new List<object>();
            syncObj.Values.Add("0000000004");
            syncObj.Values.Add("Cat56");
            //  syncObj.Values.Add("e7c22632-87e0-1fc0-cac0-cf07e1f523a3");
            syncObj.Values.Add(Guid.Parse("{e7c22632-87e0-1fc0-cac0-cf07e1f523a4}"));
            syncObj.Values.Add(null);

            syncObjList.Add(syncObj);






            var restult = x.PushData(Guid.Parse("be77804b-3406-5c47-0180-5eabac9b482d"), syncObjList, "");

        }

        private void button5_Click(object sender, EventArgs e)
        {
            XmlSerializer mySerializer = new XmlSerializer(typeof(List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>));
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = null;

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                    syncObjList = (List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>)mySerializer.Deserialize(xmlReader);
                    SynchronizationManager x = new SynchronizationManager();

                    var restult = x.PushData(syncObjList[0].AccountOID, syncObjList, "");

                }
            }
        }

        private void SwitchSchema_Click(object sender, EventArgs e)
        {
            SynchronizationManager x = new SynchronizationManager();
            x.SwitchSchema(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"));
        }








    }
}
