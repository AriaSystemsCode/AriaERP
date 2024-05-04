using Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers;
using Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling;
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
using WindowsFormsApplication1.ServiceReference3;
using Aria5SystemAdmin.Module;
using Aria5SystemAdmin.Module.Managers;
using Aria5SystemAdmin.Module.BusinessObjects;

namespace WindowsFormsApplication1
{
    public partial class Form3 : Form
    {
        public Form3()
        {
            InitializeComponent();
        }

        private void webservice_Click(object sender, EventArgs e)
        {
            ServiceReference3.AriaSynchronizationServiceSoapClient t = new ServiceReference3.AriaSynchronizationServiceSoapClient();
            
              //List< ServiceReference3.SyncDataObject> syncObjList = null;
          
            ServiceReference3.SyncDataObject status = new ServiceReference3.SyncDataObject();
            status.AccountOID = Guid.Parse("2a56d11a-e930-4b3d-90c7-f637e240603b");
            status.EntityOID = Guid.NewGuid();
            status.TableName = "EntityStatus";
            status.RecordStatus = "ADD";
            status.Properties = new  ArrayOfString();
            status.Values = new ArrayOfAnyType();
            status.Properties.Add("Oid"); status.Values.Add(status.EntityOID);
            status.Properties.Add("StatusId"); status.Values.Add("0000000030");
            status.Properties.Add("Description"); status.Values.Add("Status30");
            Guid d = Guid.Parse("E7C22632-87E0-1FC0-CAC0-CF07E1F523A3");

            status.Properties.Add("EntityType"); status.Values.Add("E7C22632-87E0-1FC0-CAC0-CF07E1F523A3");
            bool dd1 = d is Guid;
            bool dd2 = d is object;

            //syncObjList.Add((ServiceReference3.SyncDataObject)status);
          //List<ServiceReference3.SyncDataObject> datatoupdate = new List<ServiceReference3.SyncDataObject>();

            //datatoupdate=(List<ServiceReference3.SyncDataObject>)syncObjList.ToList();


            var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityStatus-----ee180f13-d239-469d-86ac-faaf1e87e727.xml", FileMode.Open, FileAccess.Read);
            XmlReader xmlReader = XmlReader.Create(fileStream);
            XmlSerializer mySerializer = new XmlSerializer(typeof(List< WindowsFormsApplication1.ServiceReference3.SyncDataObject>));
            mySerializer.CanDeserialize(xmlReader);
            var syncObjList = (List< WindowsFormsApplication1.ServiceReference3.SyncDataObject>)mySerializer.Deserialize(xmlReader);

            var res = t.PushData(Guid.Parse("2a56d11a-e930-4b3d-90c7-f637e240603b"), new [] { status} , "");




         // List<SyncDataObject> syncObjList = null;

            //XmlSerializer mySerializer = new XmlSerializer(typeof(List< SyncDataObject>));


            //using (var fileStream = new FileStream(@"C:\Users\sarah\Desktop\SyncObj-EntityStatus-----ee180f13-d239-469d-86ac-faaf1e87e727.xml", FileMode.Open, FileAccess.Read))
            //{
            //    XmlReader xmlReader = XmlReader.Create(fileStream);
            //    if (mySerializer.CanDeserialize(xmlReader))
            //    {

            //        syncObjList = (List< SyncDataObject>)mySerializer.Deserialize(xmlReader)).ToList();


                    //var restult = t.PushDataAsync(syncObjList[0].AccountOID,(List< ServiceReference3.SyncDataObject>) syncObjList, "");


            //    }
            
        }

        private void frontend_Click(object sender, EventArgs e)
        {

            SynchronizationManager1 x = new SynchronizationManager1();
           var res= x.GetMinFrontEndVersion();
        }

        private void DBVersion_Click(object sender, EventArgs e)
        {
            SynchronizationManager1 x = new SynchronizationManager1();
            var res2 = x.GetDBVersion(Guid.Parse("75B51829-66D7-4FBD-AFC0-01521D17D0DF"));
        }

        private void ClientEntity_Click(object sender, EventArgs e)
        {
            ServiceReference3.AriaSynchronizationServiceSoapClient t = new ServiceReference3.AriaSynchronizationServiceSoapClient();

            //List< ServiceReference3.SyncDataObject> syncObjList = null;

            ServiceReference3.SyncDataObject ClientEntity = new ServiceReference3.SyncDataObject();
            ClientEntity.AccountOID = Guid.Parse("2a56d11a-e930-4b3d-90c7-f637e240603b");
            ClientEntity.EntityOID = Guid.Parse("7608086c-383d-4be6-8b06-5b3df4e03a9c");
            ClientEntity.TableName = "Entity";
            ClientEntity.RecordStatus = "ADD";
            ClientEntity.Properties = new ArrayOfString();
            ClientEntity.Values = new ArrayOfAnyType();
            ClientEntity.Properties.Add("Oid"); ClientEntity.Values.Add(ClientEntity.EntityOID);
            ClientEntity.Properties.Add("EntityId"); ClientEntity.Values.Add(null);
            ClientEntity.Properties.Add("Description"); ClientEntity.Values.Add(null);
            ClientEntity.Properties.Add("Notes"); ClientEntity.Values.Add(null);
            ClientEntity.Properties.Add("Status"); ClientEntity.Values.Add("15f16437-8718-4a82-9ed1-a53c45b94450<");
            ClientEntity.Properties.Add("StatusId"); ClientEntity.Values.Add("0000000024");
            ClientEntity.Properties.Add("Type"); ClientEntity.Values.Add("e7c22632-87e0-1fc0-cac0-cf07e1f523a3");
            ClientEntity.Properties.Add("TypeId"); ClientEntity.Values.Add("NEWS");
            ClientEntity.Properties.Add("Category"); ClientEntity.Values.Add("7e06a66d-b806-451b-98ca-2b488435f713");
            ClientEntity.Properties.Add("CategoryId"); ClientEntity.Values.Add("0000000006");
            ClientEntity.Properties.Add("Classification"); ClientEntity.Values.Add("1ceef357-7418-4575-a592-50a2061c33f8");
            ClientEntity.Properties.Add("Classification"); ClientEntity.Values.Add("0000000006");
            ClientEntity.Properties.Add("ClassificationId"); ClientEntity.Values.Add("Status30");
            ClientEntity.Properties.Add("ExtraData"); ClientEntity.Values.Add("&lt;DocumentElement&gt;&lt;ExtraData&gt;&lt;Name&gt;Title&lt;/Name&gt;&lt;Value&gt;ooo&lt;/Value&gt;&lt;/ExtraData&gt;&lt;ExtraData&gt;&lt;Name&gt;Article&lt;/Name&gt;&lt;Value&gt;oooo&lt;/Value&gt;&lt;/ExtraData&gt;&lt;/DocumentElement&gt;");
            ClientEntity.Properties.Add("AddDate"); ClientEntity.Values.Add(DateTime.Now);
            ClientEntity.Properties.Add("GUID"); ClientEntity.Values.Add("78375fad-4767-46eb-9eec-7762d78b5d00");
            ClientEntity.Properties.Add("Account"); ClientEntity.Values.Add("Status30");
            var res = t.PushData(Guid.Parse("2a56d11a-e930-4b3d-90c7-f637e240603b"), new[] { ClientEntity }, "");
        }

        private void button1_Click(object sender, EventArgs e)
        {
            ConfigurationItemManager ss = new ConfigurationItemManager();
            XpoDefault.ConnectionString = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;Persist Security Info=True;User ID=azuresqladmin;Password=aria_123";
            Session session = XpoDefault.Session;
          //  string deviceSign = "120.54|222.65|162.74|76.113|150.39";
          //  string applicationID = "Aria5-Windows8Xaml-1TouchAwayFrontend";
            Guid accountOid = new Guid("87AC3FC5-4ECE-46D9-ABCF-6F031E1379ED");
            ConfigurationItem x= new ConfigurationItem(session);
            x.DemoAccount = Guid.Parse("d073240e-a607-4d1c-acae-8a294b0e7e85");
            ConfigurationItemManager.AddNewConfigurationItem(session, x, accountOid, accountOid, Guid.Parse("31BB85F6-5BAB-4A45-B1F5-51C6C2783913"), Guid.Parse("A6CD3D1F-9EA3-4C10-AF2E-1825114ECD30"));
         
        }
    }
}
