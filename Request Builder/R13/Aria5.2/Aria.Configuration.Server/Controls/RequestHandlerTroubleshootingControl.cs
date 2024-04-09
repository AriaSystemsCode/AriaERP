using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.IO;

using Aria.RequestHandler.Troubleshooting;
using System.Xml;
using System.Linq;

namespace Aria.Configuration.Server.Controls
{
    public partial class RequestHandlerTroubleshootingControl : UserControl
    {
        List<Check> rbCheckList = new List<Check>();
        string xmlFilePath;

        public RequestHandlerTroubleshootingControl()
        {
            InitializeComponent();

            Init();
        }

        public void Init()
        {
            //WriteXML();         
            //string gacPath = Environment.GetFolderPath(Environment.SpecialFolder.System) + "\\..\\assembly";
            //var files = GetGlobalAssemblyCacheFiles(gacPath);

            ReadServerSettings();

            FillCheckList();

            PopulateTreeView(null, null);
            trvChecks.ExpandAll();
        }

        private void FillCheckList()
        {
            DataSet dsChecks = new DataSet();
            dsChecks.ReadXml(xmlFilePath + @"\StandardChecks.XML", XmlReadMode.ReadSchema);
            dsChecks.ReadXml(xmlFilePath + @"\CustomChecks.XML", XmlReadMode.ReadSchema);
            foreach (DataTable dt in dsChecks.Tables)
            {
                switch (dt.TableName)
                {
                    case "FileVersionFoxScreen":            // List of Screens
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckFileVersionFoxScreen(dr["ScreenName"].ToString(), dr["ScreenModule"].ToString(), dr["ScreenPath"].ToString(), dr["CurrentVersion"].ToString()));
                        }
                        break;
                    case "FileVersionFoxClass":             // List of Classes
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckFileVersionFoxClass(dr["ClassName"].ToString(), dr["ClassModule"].ToString(), dr["ClassPath"].ToString(), dr["CurrentVersion"].ToString()));
                        }
                        break;
                    case "FileVersionFoxProgram":           // List of Report Programs                    
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckFileVersionFoxProgram(dr["ProgramName"].ToString(), dr["ProgramModule"].ToString(), dr["ProgramPath"].ToString(), dr["CurrentVersion"].ToString()));
                        }
                        break;
                    case "FileVersionFoxServerProgram":     // List of Server Programs
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckFileVersionFoxServerProgram(dr["ProgramName"].ToString(), dr["ProgramModule"].ToString(), dr["ProgramPath"].ToString(), dr["CurrentVersion"].ToString()));
                        }
                        break;
                    case "FileVersionAssembly":             // List of Assemblies
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckFileVersionAssembly(dr["AssemblyName"].ToString(), dr["AssemblyGUID"].ToString(), dr["CurrentVersion"].ToString()));
                        }
                        break;
                    case "DataBaseVersion":                 // List of Database Version
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckDatabaseVersion(dr["DatabaseName"].ToString(), dr["VersionName"].ToString(), dr["CurrentVersion"].ToString()));
                        }
                        break;
                    case "OldAssemblyInGAC":
                        foreach (DataRow dr in dt.Rows)
                        {
                            rbCheckList.Add(new CheckAssemblyExistsInGac(dr["AssemblyName"].ToString()));
                        }
                        break;
                    default:
                        break;
                }
            }
        }

        private void PopulateTreeView(Check ParentCheck, TreeNode parentNode)
        {
            var filteredItems = rbCheckList.Where(item => item.ParentCheck == ParentCheck);

            TreeNode childNode;
            foreach (var checkItem in filteredItems.ToList())
            {
                if (parentNode == null)
                    childNode = trvChecks.Nodes.Add(checkItem.CheckAction);
                else
                    childNode = parentNode.Nodes.Add(checkItem.CheckAction);

                PopulateTreeView(checkItem, childNode);
            }
        }

        private void btnStartCheck_Click(object sender, EventArgs e)
        {
            // Reset Code 
            prbCheckProgress.Maximum = rbCheckList.Count;
            prbCheckProgress.Value = 0;
            foreach (Check rbCheck in rbCheckList)
            {
                rbCheck.CheckResult = CheckResult.NotChecked;
                rbCheck.CheckError = "";
                TreeNode resultNode = SearchNode(rbCheck.CheckAction, trvChecks.Nodes[0]);
                resultNode.ImageIndex = (int)rbCheck.CheckResult;
                resultNode.SelectedImageIndex = (int)rbCheck.CheckResult;
                resultNode.ToolTipText = rbCheck.CheckError;
            }

            //rbCheckList.ForEach(item => item.GetCheckResult());

            foreach (Check rbCheck in rbCheckList)
            {
                rbCheck.GetCheckResult();
                TreeNode resultNode = SearchNode(rbCheck.CheckAction, trvChecks.Nodes[0]);
                resultNode.ImageIndex = (int)rbCheck.CheckResult;
                resultNode.SelectedImageIndex = (int)rbCheck.CheckResult;
                resultNode.ToolTipText = rbCheck.CheckError;
                prbCheckProgress.Value += 1;
            }
        }

        private TreeNode SearchNode(string SearchText, TreeNode StartNode)
        {
            TreeNode node = null;
            while (StartNode != null)
            {
                if (StartNode.Text.ToLower().Contains(SearchText.ToLower()))
                {
                    node = StartNode;
                    break;
                };
                if (StartNode.Nodes.Count != 0)
                {
                    node = SearchNode(SearchText, StartNode.Nodes[0]);//Recursive Search
                    if (node != null)
                    {
                        break;
                    };
                };
                StartNode = StartNode.NextNode;
            };
            return node;
        }

        private void GetAria4SharedPath()
        {
            FolderBrowserDialog fldBrowse = new FolderBrowserDialog();
            if (fldBrowse.ShowDialog() == DialogResult.OK)
            {
                txtAria4SharedPath.Text = fldBrowse.SelectedPath + @"\";
                AriaVars.Aria4SharedPath = fldBrowse.SelectedPath + @"\";
            }
        }

        private void btnPathBrowse_Click(object sender, EventArgs e)
        {
            GetAria4SharedPath();
        }

        private void txtAria4SharedPath_Leave(object sender, EventArgs e)
        {
            if (!Directory.Exists(txtAria4SharedPath.Text))
            {
                txtAria4SharedPath.Text = "";
                GetAria4SharedPath();
            }
            else
            {
                if (txtAria4SharedPath.Text.PadLeft(1) != @"\")
                {
                    txtAria4SharedPath.Text += @"\";
                }
                AriaVars.Aria4SharedPath = txtAria4SharedPath.Text;
            }
        }

        private void ReadServerSettings()
        {
            string xmlFileName = System.IO.Path.GetDirectoryName(Application.ExecutablePath) + @"\configuration settings.xml";
            if (File.Exists(xmlFileName))
            {
                AriaVars.cAssemblyPath = System.IO.Path.GetDirectoryName(Application.ExecutablePath);
            }
            else
            {
                openFileDialog1.FileName = System.Environment.GetEnvironmentVariable("ProgramFiles") + @"\Aria Systems Inc\Aria5\configuration settings.xml";
                if (openFileDialog1.ShowDialog() == DialogResult.OK)
                {
                    xmlFileName = openFileDialog1.FileName;
                    AriaVars.cAssemblyPath = System.IO.Path.GetDirectoryName(openFileDialog1.FileName);
                }
                else
                {
                    AriaVars.cAssemblyPath = System.IO.Path.GetDirectoryName(Application.ExecutablePath);
                }
            }

            xmlFilePath = AriaVars.cAssemblyPath;

            XmlDocument xmlDocument = new XmlDocument();

            xmlDocument.Load(xmlFileName);

            XmlElement documentElement = xmlDocument.DocumentElement;

            for (int index = 0; index < xmlDocument.DocumentElement.ChildNodes.Count; index++)
            {
                if (documentElement.ChildNodes[index].Name == "FileServer")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        if (xmlNode.Name == "Aria40SharedPath")
                        {
                            txtAria4SharedPath.Text = xmlNode.InnerText.TrimEnd();
                            if (!txtAria4SharedPath.Text.EndsWith(@"\"))
                            {
                                txtAria4SharedPath.Text += @"\";
                            }
                        }
                    }
                }
                if (documentElement.ChildNodes[index].Name == "DatabaseSetup")
                {
                    XmlNode xmlNode = null;

                    for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                    {
                        xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                        switch (xmlNode.Name)
                        {
                            case "ServerName":
                                AriaVars.ServerName = xmlNode.InnerText.TrimEnd();
                                break;
                            case "ServerLoginType":
                                AriaVars.ServerLoginType = (xmlNode.InnerText.TrimEnd() == "SqlServerAuthentication") ? DBLoginTypes.SqlServerAuthentication : DBLoginTypes.WindowAuthentication;
                                break;
                            case "UserName":
                                AriaVars.UserName = xmlNode.InnerText.TrimEnd();
                                break;
                            case "Password":
                                AriaVars.Password = xmlNode.InnerText.TrimEnd();
                                break;
                            default:
                                break;
                        }

                    }
                }
            }
            AriaVars.Aria4SharedPath = txtAria4SharedPath.Text;
        }

        public static string[] GetGlobalAssemblyCacheFiles(string path)
        {
            List<string> files = new List<string>();

            DirectoryInfo di = new DirectoryInfo(path);

            foreach (FileInfo fi in di.GetFiles("Aria*.dll"))
            {
                files.Add(fi.FullName);
            }

            foreach (DirectoryInfo diChild in di.GetDirectories())
            {
                var files2 = GetGlobalAssemblyCacheFiles(diChild.FullName);
                files.AddRange(files2);
            }

            return files.ToArray();
        }

        private void WriteXML()
        {
            DataSet ds = new DataSet();
            DataTable dtFoxScreen = new DataTable("FileVersionFoxScreen");
            DataTable dtFoxClass = new DataTable("FileVersionFoxClass");
            DataTable dtFoxProgram = new DataTable("FileVersionFoxProgram");
            DataTable dtFoxServerProgram = new DataTable("FileVersionFoxServerProgram");
            DataTable dtAssembly = new DataTable("FileVersionAssembly");
            DataTable dtDataBase = new DataTable("DataBaseVersion");
            DataTable dtOldAssembly = new DataTable("OldAssemblyInGAC");

            dtFoxScreen.Columns.AddRange(new DataColumn[] { new DataColumn("ScreenName"), new DataColumn("ScreenModule"), new DataColumn("ScreenPath"), new DataColumn("CurrentVersion") });
            dtFoxClass.Columns.AddRange(new DataColumn[] { new DataColumn("ClassName"), new DataColumn("ClassModule"), new DataColumn("ClassPath"), new DataColumn("CurrentVersion") });
            dtFoxProgram.Columns.AddRange(new DataColumn[] { new DataColumn("ProgramName"), new DataColumn("ProgramModule"), new DataColumn("ProgramPath"), new DataColumn("CurrentVersion") });
            dtFoxServerProgram.Columns.AddRange(new DataColumn[] { new DataColumn("ProgramName"), new DataColumn("ProgramModule"), new DataColumn("ProgramPath"), new DataColumn("CurrentVersion") });
            dtAssembly.Columns.AddRange(new DataColumn[] { new DataColumn("AssemblyName"), new DataColumn("AssemblyGUID"), new DataColumn("CurrentVersion") });
            dtDataBase.Columns.AddRange(new DataColumn[] { new DataColumn("DatabaseName"), new DataColumn("VersionName"), new DataColumn("CurrentVersion") });
            dtOldAssembly.Columns.AddRange(new DataColumn[] { new DataColumn("AssemblyName") });

            // List of Screens
            dtFoxScreen.Rows.Add(new object[] { "Request Screen", "SM", @"SM\Request.SCX", "3.0.0.0" });
            dtFoxScreen.Rows.Add(new object[] { "Request Monitor Screen", "SM", @"SM\ReqMont.SCX", "3.0.0.0" });
            dtFoxScreen.Rows.Add(new object[] { "Request Mail Screen", "SM", @"SM\ReqMail.SCX", "3.0.0.0" });
            dtFoxScreen.Rows.Add(new object[] { "Request Tasks Screen", "SM", @"SM\ReqTasks.SCX", "3.0.0.0" });

            // List of Classes
            dtFoxClass.Rows.Add(new object[] { "Aria Main Class", "SY", @"SY\AriaMain.VCX", "3.0.0.0" });
            dtFoxClass.Rows.Add(new object[] { "Request Handler Class", "SY", @"SY\RequestHandler.VCX", "3.0.0.0" });

            // List of Server Programs
            dtFoxServerProgram.Rows.Add(new object[] { "Aria Main Program", "SY", @"SY\AriaMain.FXP", "3.0.0.0" });
            dtFoxServerProgram.Rows.Add(new object[] { "Run Code Program", "SY", @"SY\RunCode.FXP", "3.0.0.0" });

            // List of Report Programs            
            dtFoxProgram.Rows.Add(new object[] { "Codes Report Program", "SM", @"SM\SMCCODE.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Invoice Report Program", "AR", @"ARPINV.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Credit Memo Report Program", "RM", @"RMCMEM.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Style Purchase Order Report Program", "PO", @"POSTYP.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Order Confirmation Report Program", "SO", @"SO\SOORCN.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Order Header Report Program", "SO", @"SO\SOORDHD.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Customer Statement Report Program", "AR", @"AR\ARCSTM.FXP", "3.0.0.0" });
            dtFoxProgram.Rows.Add(new object[] { "Picking Ticket Report Program", "AL", @"AL\ALPKTK.FXP", "3.0.0.0" });

            // List of Assemblies
            dtAssembly.Rows.Add(new object[] { "Aria.Data", "2.0.0.0__094d51c8c9be7436", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Data.BusinessObject", "2.0.0.0__ed4b489f59590957", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Data.DataTypes", "2.0.0.0__af37fb98f6a57fae", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.DataAccessLayer", "1.0.0.0__63515e805a807ba5", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.DataTypes", "2.0.0.0__50dbf4009ba40281", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.DataTypes.Messaging", "2.0.0.0__d9d50c148c28e6e5", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.DataTypes.ObjectDictionary", "2.0.0.0__eb16ea7e41946605", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.DataTypes.RequestHandler", "2.0.0.0__6738bd32cab75706", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.EnterpriseServices.Messaging", "2.0.0.0__e67b3c58fc8c22cb", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.EnterpriseServices.ObjectDictionary", "2.0.0.0__6c192ac0cc49d834", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.EnterpriseServices.RequestHandler.Proxy", "2.0.0.0__46c3ceff3ffa604e", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Environment", "2.0.0.0__335898ba5fbd5bb5", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.HelperClass.SAAS", "1.0.0.0__6cebc7b795ae6c8c", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.ObjectDictionary.UI", "2.0.0.0__763fb17306c7328c", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Reflection", "2.0.0.0__8e2e344217a5a99f", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Utilities.Log", "1.0.0.0__fdb14855d5b07748", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Utilities.ParameterSubstitution", "2.0.0.0__808761383e6d855c", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Utilities.RemoteCall", "1.0.0.0__514f62c7a39b8e55", "3.0.0.0" });
            dtAssembly.Rows.Add(new object[] { "Aria.Xml", "2.0.0.0__8da0fd77c710998c", "3.0.0.0" });

            // List Database Versions
            dtDataBase.Rows.Add(new object[] { "System.Master", "Version", "27" });
            dtDataBase.Rows.Add(new object[] { "Aria.Master", "Version", "26" });
            dtDataBase.Rows.Add(new object[] { "Aria.Master", "CustomizationVersion", "1" });

            // List of Assemblies
            dtOldAssembly.Rows.Add(new object[] { "Aria.Data" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Data.BusinessObject" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Data.DataTypes" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.DataAccessLayer" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.DataTypes" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.DataTypes.Messaging" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.DataTypes.ObjectDictionary" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.DataTypes.RequestHandler" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.EnterpriseServices.Messaging" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.EnterpriseServices.ObjectDictionary" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.EnterpriseServices.RequestHandler.Proxy" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Environment" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.HelperClass.SAAS" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.ObjectDictionary.UI" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Reflection" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Utilities.Log" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Utilities.ParameterSubstitution" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Utilities.RemoteCall" });
            dtOldAssembly.Rows.Add(new object[] { "Aria.Xml" });

            ds.Tables.AddRange(new DataTable[] { dtFoxScreen, dtFoxClass, dtFoxProgram, dtFoxServerProgram, dtAssembly, dtDataBase, dtOldAssembly });

            ds.WriteXml(@"C:\Test.XML", XmlWriteMode.WriteSchema);
        }

        private void ReadXML()
        {
            DataSet ds2 = new DataSet();
            ds2.ReadXml(@"C:\Test.XML", XmlReadMode.ReadSchema);
        }        
    }
}
