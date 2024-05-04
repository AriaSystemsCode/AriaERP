using Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.WebServices;
using Aria5SystemAdmin.Module;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Xml;
using System.Xml.Serialization;

namespace WindowsFormsApplication1
{
    public partial class Form2 : Form
    {
        public Form2()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();

            SyncDataObject entityType = new SyncDataObject();
            // Mahmoud
            //entityType.EntityOID = Guid.Parse("C6F43720-A3EA-4506-83F5-AF501FF55291");
            entityType.EntityOID = Guid.NewGuid();
            // Mahmoud
            entityType.TableName = "EntityType";
            entityType.RecordStatus = "ADD";
            entityType.Properties = new List<string>();
            entityType.Values = new List<object>();
            entityType.Properties.Add("Oid"); entityType.Values.Add(entityType.EntityOID);
            entityType.Properties.Add("TypeId"); entityType.Values.Add("TYPE11");
            entityType.Properties.Add("Name"); entityType.Values.Add("Type11");
            x.PushData(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"), new List<SyncDataObject>() { entityType }, "");

            SyncDataObject status = new SyncDataObject();
            status.EntityOID = Guid.NewGuid();
            status.TableName = "EntityStatus";
            status.RecordStatus = "ADD";
            status.Properties = new List<string>();
            status.Values = new List<object>();
            status.Properties.Add("Oid"); status.Values.Add(status.EntityOID);
            status.Properties.Add("StatusId"); status.Values.Add("STATUS1");
            status.Properties.Add("Description"); status.Values.Add("Status1");
            status.Properties.Add("EntityType"); status.Values.Add(entityType.EntityOID);
            x.PushData(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"), new List<SyncDataObject>() { status }, "");

            SyncDataObject Category = new SyncDataObject();
            Category.EntityOID = Guid.NewGuid();
            Category.TableName = "EntityCategory";
            Category.RecordStatus = "ADD";
            Category.Properties = new List<string>();
            Category.Values = new List<object>();
            Category.Properties.Add("Oid"); Category.Values.Add(Category.EntityOID);
            Category.Properties.Add("CategoryId"); Category.Values.Add("Category1");
            Category.Properties.Add("Name"); Category.Values.Add("Category1");
            Category.Properties.Add("EntityType"); Category.Values.Add(entityType.EntityOID);
            Category.Properties.Add("ParentCategory"); Category.Values.Add(Category.EntityOID);

            x.PushData(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"), new List<SyncDataObject>() { Category }, "");



            SyncDataObject Classification = new SyncDataObject();
            Classification.EntityOID = Guid.NewGuid();
            Classification.RecordStatus = "ADD";
            Classification.TableName = "EntityClassification";
            Classification.Properties = new List<string>();
            Classification.Values = new List<object>();
            Classification.Properties.Add("Oid"); Classification.Values.Add(Classification.EntityOID);
            Classification.Properties.Add("ClassificationID"); Classification.Values.Add("Classification1");
            Classification.Properties.Add("Name"); Classification.Values.Add("Classification1");
            Classification.Properties.Add("EntityType"); Classification.Values.Add(entityType.EntityOID);
            Classification.Properties.Add("ParentClassification"); Classification.Values.Add(Classification.EntityOID);

            x.PushData(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"), new List<SyncDataObject>() { Classification }, "");


            SyncDataObject News = new SyncDataObject();
            News.EntityOID = Guid.NewGuid();
            News.RecordStatus = "ADD";
            News.TableName = "News";
            News.Properties = new List<string>();
            News.Values = new List<object>();
            News.Properties.Add("Oid"); News.Values.Add(News.EntityOID);
            News.Properties.Add("Subject"); News.Values.Add("testnews1");
            News.Properties.Add("Article"); News.Values.Add("testnews2");
            News.Properties.Add("Description"); News.Values.Add("testnews333");
            News.Properties.Add("Status"); News.Values.Add(status.EntityOID);
            News.Properties.Add("Category"); News.Values.Add(Category.EntityOID);
            News.Properties.Add("Classification"); News.Values.Add(Classification.EntityOID);
            News.Properties.Add("AddDate"); News.Values.Add(DateTime.Now);
            News.Properties.Add("Division"); News.Values.Add(Guid.Parse("F8BDA0DD-A4A8-49CB-AB4D-0A43563FC892"));




        }

        private void pull_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
            var result = x.PullData("0x0000000000011129", "0x0000000000011132", Guid.Parse("2a56d11a-e930-4b3d-90c7-f637e240603b"));

        }

        private void EntityType_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
            XmlSerializer mySerializer = new XmlSerializer(typeof(List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>));
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = null;

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj- EntityType.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                    syncObjList = (List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>)mySerializer.Deserialize(xmlReader);


                    var restult = x.PushData(syncObjList[0].AccountOID, syncObjList, "");

                }
            }

        }

        private void EntityStatus_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
            XmlSerializer mySerializer = new XmlSerializer(typeof(List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>));
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = null;

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityStatus-----ee180f13-d239-469d-86ac-faaf1e87e727.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                    syncObjList = (List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>)mySerializer.Deserialize(xmlReader);


                    var restult = x.PushData(syncObjList[0].AccountOID, syncObjList, "");

                }
            }
        }

        private void entityattach_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
            XmlSerializer mySerializer = new XmlSerializer(typeof(List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>));
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = null;

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityClassification-----de97404c-5ce7-4ecb-95fd-3863039f6e53.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                    syncObjList = (List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>)mySerializer.Deserialize(xmlReader);


                    var restult = x.PushData(syncObjList[0].AccountOID, syncObjList, "");

                }
            }
        }

        private async void webservice_Click(object sender, EventArgs e)
        {
            ServiceReference3.AriaSynchronizationServiceSoapClient t = new ServiceReference3.AriaSynchronizationServiceSoapClient();


            List<WindowsFormsApplication1.ServiceReference3.SyncDataObject> syncObjList = null;
            XmlSerializer mySerializer = new XmlSerializer(typeof(List<WindowsFormsApplication1.ServiceReference3.SyncDataObject>));
        

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityClassification-----de97404c-5ce7-4ecb-95fd-3863039f6e53.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                   
                    syncObjList.Add((ServiceReference3.SyncDataObject)mySerializer.Deserialize(xmlReader));
                    

                    var restult = await t.PushDataAsync(syncObjList[0].AccountOID, syncObjList.ToArray(), "");


                }
            }
        }

            

        private void button2_Click(object sender, EventArgs e)
        {
            AriaSynchronizationService t = new AriaSynchronizationService();

            XmlSerializer mySerializer = new XmlSerializer(typeof(List<SyncDataObject>));
            List<SyncDataObject> syncObjList = null;

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityCategory1.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                    syncObjList.Add ((SyncDataObject)mySerializer.Deserialize(xmlReader));

                    var restult = t.PushData(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"), syncObjList, "");

                }
            }
        } 

        private void CopySchema_Click(object sender, EventArgs e)
        {
            DataManger DM = new DataManger();
            DM.CopySchema(Guid.Parse("D073240E-A607-4D1C-ACAE-8A294B0E7E85"), Guid.Parse("A6CD3D1F-9EA3-4C10-AF2E-1825114ECD30"), "REDLOB");
       //AriaSynchronizationService x= new AriaSynchronizationService()
        }

        private void settings_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
            XmlSerializer mySerializer = new XmlSerializer(typeof(List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>));
            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> syncObjList = null;

            using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityTypeSettings-----80c23b01-58b5-43c1-9431-8730f2558ad7.xml", FileMode.Open, FileAccess.Read))
            {
                XmlReader xmlReader = XmlReader.Create(fileStream);
                if (mySerializer.CanDeserialize(xmlReader))
                {
                    syncObjList = (List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>)mySerializer.Deserialize(xmlReader);

                    var restult = x.PushData(syncObjList[0].AccountOID, syncObjList, "");

                }
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
          //  x.GetMinFrontEndVersion();
            var res = x.GetDBVersion(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"));
        }

        private void button4_Click(object sender, EventArgs e)
        {

            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123";
            Session session = XpoDefault.Session;

            AccountManager acc = new AccountManager();

            var result =     acc.GetAllDemoAccounts();
        }

        private void button5_Click(object sender, EventArgs e)
        {

            AriaUserWebServiceRefrence.AriaUserWebServiceSoapClient userWs = new AriaUserWebServiceRefrence.AriaUserWebServiceSoapClient();
            Guid accountOid = userWs.UserAuthenticationAsync("hassan.i20@ariasystems.biz", "aria_123").Result.Body.UserAuthenticationResult;

        }

        private void button6_Click(object sender, EventArgs e)
        {

            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123";
            Session session = XpoDefault.Session;

            
            var result = AriaUserManager.UserAuthentication("hassan.i20@ariasystems.biz", "aria_123");




        }

        private void Form2_Load(object sender, EventArgs e)
        {

        }
    }
}

