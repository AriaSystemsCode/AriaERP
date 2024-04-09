using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Aria.HelperClass.SAAS;
using System.IO;
using Aria.Environment;
using Aria.EnterpriseServices.Messaging;
using System.Diagnostics;
using System.Data.SqlClient;
using System.Reflection;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels.Tcp;
using System.Runtime.Remoting.Channels;
using Aria.Data;
using System.Runtime.Remoting.Channels.Http;
using System.Collections;
using System.Runtime.Serialization.Formatters;
using Aria.EnterpriseServices.RequestHandler;
using Aria.Utilities.RemoteCall;

namespace Test
{
    public partial class Form1 : Form
    {
        private TcpChannel mi_Channel = null;
        
        private ObjRef objRefRequestAgent = null;
        private AriaRequestAgent requestAgent = null;
        private ObjRef objRefObjectDictionaryDBCentric = null;
        private AriaObjectDictionaryDBCentric objectDictionaryDBCentric = null;

        private ObjRef objRefConnection = null;
        private AriaDbConnection connection = null;

        public Form1()
        {
            InitializeComponent();

        }

        private void button1_Click(object sender, EventArgs e)
        {
            Aria.Environment.AriaEnviromentVariables x = new AriaEnviromentVariables();
            //string w = x.GetAbsolutePath(@"R:\");

            File.WriteAllText(Path.Combine(@"c:\", "Client_Setting.xml"), "a");

            //lo.LoadAriaObjectChildObjectsOfType(loConnection, "Aria4XP", 1, "VIS20")
            
            //Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric w = new Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric();

            //object www = w.LoadAriaObjectChildObjectsOfType(new Aria.Data.AriaDbConnection(), "ARIA4XP", Aria.DataTypes.ObjectDictionary.AriaObjectTypes.Data, "VIS20");

            //SqlCommand command = new SqlCommand(@"SELECT DISTINCT [Aria4XP.Style Purchase Order].[CBUSDOCU] AS [CBUSDOCU], [Aria4XP.Style Purchase Order].[PO] AS [PO], [Aria4XP.Style Purchase Order].[CSTYTYPE] AS [CSTYTYPE] FROM OPENROWSET('MSDASQL','Driver={SQL Server};server=.\SQLSERVER2008R2;DATABASE=DEMO_LDB99;UID=;PWD=','SELECT * FROM POSHDR') AS [Aria4XP.Style Purchase Order] LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\aria27\sysfiles\;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM SYUUSER') AS [Aria4XP.Style Purchase Order.Add User] ON  [Aria4XP.Style Purchase Order.Add User].[Cuser_id] = [Aria4XP.Style Purchase Order].[Cadd_user] LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\Aria27\DBFs\99;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM CUSTOMER') AS [Aria4XP.Style Purchase Order.Customer] ON  [Aria4XP.Style Purchase Order.Customer].[ACCOUNT] = [Aria4XP.Style Purchase Order].[ACCOUNT] AND [Aria4XP.Style Purchase Order.Customer].[STORE] = [Aria4XP.Style Purchase Order].[STORE] LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\Aria27\DBFs\99;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM CODES') AS [Aria4XP.Style Purchase Order.Division] ON  [Aria4XP.Style Purchase Order.Division].[CDEFCODE] = 'N' AND [Aria4XP.Style Purchase Order.Division].[CCODE_NO] = [Aria4XP.Style Purchase Order].[CDIVISION] AND [Aria4XP.Style Purchase Order.Division].[CRLTFIELD] = 'N' AND [Aria4XP.Style Purchase Order.Division].[CFLD_NAME] = 'CDIVISION' LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\aria27\sysfiles\;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM SYUUSER') AS [Aria4XP.Style Purchase Order.Edit User] ON  [Aria4XP.Style Purchase Order.Edit User].[Cuser_id] = [Aria4XP.Style Purchase Order].[Cedit_user] LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\Aria27\DBFs\99;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM CODES') AS [Aria4XP.Style Purchase Order.Payment Terms] ON  [Aria4XP.Style Purchase Order.Payment Terms].[CDEFCODE] = 'N' AND [Aria4XP.Style Purchase Order.Payment Terms].[CCODE_NO] = [Aria4XP.Style Purchase Order].[CTERMCODE] AND [Aria4XP.Style Purchase Order.Payment Terms].[CRLTFIELD] = 'N' AND [Aria4XP.Style Purchase Order.Payment Terms].[CFLD_NAME] = 'CTERMCODE' LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\Aria27\DBFs\99;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM CODES') AS [Aria4XP.Style Purchase Order.Ship Via] ON  [Aria4XP.Style Purchase Order.Ship Via].[CDEFCODE] = 'N' AND [Aria4XP.Style Purchase Order.Ship Via].[CCODE_NO] = [Aria4XP.Style Purchase Order].[SHIPVIA] AND [Aria4XP.Style Purchase Order.Ship Via].[CRLTFIELD] = 'N' AND [Aria4XP.Style Purchase Order.Ship Via].[CFLD_NAME] = 'SHIPVIA' LEFT JOIN  OPENROWSET('MSDASQL','Driver={Microsoft Visual FoxPro Driver};sourcedb=d:\Aria27\DBFs\99;sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes','SELECT * FROM APVENDOR') AS [Aria4XP.Style Purchase Order.Vendor] ON  [Aria4XP.Style Purchase Order.Vendor].[CVENDCODE] = [Aria4XP.Style Purchase Order].[VENDOR]");

            //command.CommandTimeout = 0;

            //command.Connection = new SqlConnection(@"Data Source=.\SQLSERVER2008R2;uid=admin;pwd=myaria_1234");

            //command.Connection.Open();

            //DataTable table = new DataTable();

            //SqlDataAdapter adapter = new SqlDataAdapter(command);

            //adapter.Fill(table);


            //List<string> a = new List<string>();
            //foreach (System.IO.DriveInfo di in System.IO.DriveInfo.GetDrives())
            //{
            //    a.Add(di.Name);
            //}
            //System.IO.File.WriteAllLines(@"d:\a2.txt", a.ToArray());


            //183d0526-07d1-47c8-8770-601053a07b9d

            //Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy xxxx = new Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy();
            //xxxx.RemoveRequest("183d0526-07d1-47c8-8770-601053a07b9d", "VIS20");
            //    return;

            //Aria.EnterpriseServices.RequestHandler.AriaRequestAgent y = new Aria.EnterpriseServices.RequestHandler.AriaRequestAgent();
            //y.GenerateScheduleRequests("VIS20");
            //y.ExecuteOneTimeOnlyRequests("VIS20");
            //return;

            //AriaEnviromentVariables x = new AriaEnviromentVariables();
            //x.ClientID = "VIS20";
            //x.CompanyID = "99";

            //DataTable d = new DataTable("ClientConfig");
            //d.Columns.Add("ClientID");
            //DataRow r = d.NewRow();
            //r[0] = "BRI10";
            //d.Rows.Add(r);
            //d.AcceptChanges();
            //d.WriteXml("C:\\c1.XML", XmlWriteMode.WriteSchema);
            //d.ReadXml("C:\\c.XML");

            //AriaMessagingManager m = new AriaMessagingManager();
            //m.SMTPServer = "smtp.gmail.com";
            //m.SMTPPort = 587;
            //m.SMTPUsername = "ahmed.maher@ariany.com";
            //m.SMTPPass = "javasoft";
            //m.EmailSubject = "Test";
            //m.EmailBody = "Test body";
            //m.EnableSsl = true;
            //m.SendEmail("telecom_ahmed@yahoo.com;ahmed.maher@ariany.com", "", "", "");

            //AriaLogManage logm = new AriaLogManage();
            

            //DataSet DS = new DataSet();
            //DS.ReadXml("C:\\D.xml");
            //MessageBox.Show(x.GetClientID());
            //AriaClient c = new AriaClient();
            //SystemMasterDataSet.CLIENTSDataTable t = new SystemMasterDataSet.CLIENTSDataTable();
            //SystemMasterDataSet.CLIENTSRow w = t.NewCLIENTSRow();
            //w.CCLIENTID = "mm";
            //w.CCLIENTNAME = "mm name Updated";
            //c.UpdateClient(w);
            AriaEnviromentVariables env = new AriaEnviromentVariables();
            //Process process = new Process();
            //process.StartInfo.FileName = Application.StartupPath + "\\RegAsm.exe";
            //process.StartInfo.Arguments = "/if \"" + @"C:\Aria4XP\b609035.oexe" + "\"";

            //process.StartInfo.CreateNoWindow = true;
            //process.StartInfo.UseShellExecute = false;
            //process.StartInfo.RedirectStandardOutput = true;

            //process.Start();

            //process.WaitForExit();

        }

        public object GetRemoteObject(string className, string computerName, int portNumber)
        {
            string url = string.Format("tcp://{0}:{1}/{2}", computerName, portNumber, className);

            foreach (AssemblyName assemblyName in Assembly.GetExecutingAssembly().GetReferencedAssemblies())
            {
                Assembly assembly = Assembly.Load(assemblyName);

                foreach (Type type in assembly.GetTypes())
                {
                    if (type.FullName == className)
                    {
                        return Activator.GetObject(type, url);
                    }
                }
            }
            return null;
        }

        private void button2_Click(object sender, EventArgs e)
        {

            BinaryClientFormatterSinkProvider clientProvider = new BinaryClientFormatterSinkProvider();
            BinaryServerFormatterSinkProvider serverProvider = new BinaryServerFormatterSinkProvider();
            serverProvider.TypeFilterLevel =System.Runtime.Serialization.Formatters.TypeFilterLevel.Full;

            IDictionary props = new Hashtable();
            props["port"] = 0;
            string s = System.Guid.NewGuid().ToString();
            props["name"] = s;
            props["typeFilterLevel"] = TypeFilterLevel.Full;
            TcpChannel chan = new TcpChannel(props, clientProvider, serverProvider);

            ChannelServices.RegisterChannel(chan);




            AriaDbConnection ww = new Aria.Data.AriaDbConnection();
            AriaObjectDictionaryDBCentric x = (AriaObjectDictionaryDBCentric)GetRemoteObject("Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric", "DEV-M-SAID", 1500);
            object xx =   x.LoadAriaObjects(ww, "VIS20");

            //AriaRequestAgent w = (AriaRequestAgent)GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent", "DEV-M-SAID", 1500);
            //w.GenerateScheduleRequests("VIS20");
        }

        private void button3_Click(object sender, EventArgs e)
        {
            BinaryServerFormatterSinkProvider bcfs = new BinaryServerFormatterSinkProvider();
            bcfs.TypeFilterLevel = TypeFilterLevel.Full;
            IDictionary id = new Hashtable();
            id = new Hashtable();
            id.Add("port", 1500);
            mi_Channel = new TcpChannel(id, null, bcfs);

            ChannelServices.RegisterChannel(mi_Channel, false);

            objectDictionaryDBCentric = new AriaObjectDictionaryDBCentric();
            objRefObjectDictionaryDBCentric = RemotingServices.Marshal(objectDictionaryDBCentric, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric");

            return;
            //requestAgent = new AriaRequestAgent();
            //objRefRequestAgent = RemotingServices.Marshal(requestAgent, "Aria.EnterpriseServices.RequestHandler.AriaRequestAgent");

            //BinaryServerFormatterSinkProvider serverProv = new BinaryServerFormatterSinkProvider();
            //serverProv.TypeFilterLevel = System.Runtime.Serialization.Formatters.TypeFilterLevel.Full;

            //BinaryClientFormatterSinkProvider clientProv = new BinaryClientFormatterSinkProvider();

            //IDictionary props = new Hashtable();
            //props["port"] = 1500;


            //HttpChannel chan = new HttpChannel(props, clientProv, serverProv);
            //ChannelServices.RegisterChannel(chan);

            int s32_Port = int.Parse("1500");
            RemotingConfiguration.CustomErrorsMode = CustomErrorsModes.Off;
            mi_Channel = new TcpChannel(s32_Port);
            //mi_Channel.IsSecured = true;

            ChannelServices.RegisterChannel(mi_Channel);



            //connection = new AriaDbConnection();
            //objRefConnection = RemotingServices.Marshal(connection, "Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric");
        }

        private void button4_Click(object sender, EventArgs e)
        {
            AriaActivator df = new AriaActivator();
            object tt = df.GetRemoteObject("Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric", "DEV-M-SAID", 1500);
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            Class1 y = new Class1();
            y.A2 = "ddd";
            Aria.Xml.AriaXmlSerializer w = new Aria.Xml.AriaXmlSerializer();
            string a1 = w.ConvertToXml(y);

            y = (Class1)w.ConvertFromXml(a1);

        }
    }
}
