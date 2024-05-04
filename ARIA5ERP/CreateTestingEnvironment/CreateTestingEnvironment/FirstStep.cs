using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Data.SqlClient;
using System.Data.OleDb;
using System.Xml;
using System.IO;
using Microsoft.SqlServer.Management.Common;
using Microsoft.SqlServer.Management.Smo;
using System.Threading;
using aejw.Network;
using System.Diagnostics;
using IWshRuntimeLibrary;
namespace CreateTestingEnvironment
{
    public partial class Form1 : Form
    {
        DataSet ds = new DataSet();
        DataSet dsComp = new DataSet();
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            //Aria.Utilities.RemoteCall.AriaActivator loActivator = new Aria.Utilities.RemoteCall.AriaActivator ();
            //Aria.Environment.AriaEnviromentVariables ariaEnv = (Aria.Environment.AriaEnviromentVariables)loActivator.GetRemoteObject("Aria.Environment.AriaEnviromentVariables", Environment.MachineName, 1500);
            //string clientsConnectionString = ariaEnv.Aria50SystemFilesConnectionString;
            string livesqlserver = "", liveuserName = "", livePassword = "", destinationFolder = "", dBFolder = "", backupFolder="";
            string sqlserver = "", userName = "", password = "";
            try
            {
                XmlReader reader = XmlReader.Create(System.Windows.Forms.Application.StartupPath + @"\CreateTestingEnvironmentsettings.xml");
                while (reader.Read())
                {
                    if (reader.NodeType == XmlNodeType.Element && reader.Name == "settings")
                    {
                        while (reader.NodeType != XmlNodeType.EndElement)
                        {
                            reader.Read();
                            switch ((reader.Name))
                            {
                                case "DestinationFolder":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        destinationFolder = reader.Value;
                                    }
                                    break;
                                case "DataBaseFolder":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        dBFolder = reader.Value;
                                    }
                                    break;
                                case "BackupFolder":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        backupFolder = reader.Value;
                                    }
                                    break;


                                case "Sqlserver":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        sqlserver = reader.Value;
                                    }
                                    break;
                                case "UserName":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        userName = reader.Value;
                                    }
                                    break;
                                case "Password":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        password = reader.Value;
                                    }
                                    break;
                                case "LivePassword":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        livePassword = reader.Value;
                                    }
                                    break;
                                case "LiveUserName":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        liveuserName = reader.Value;
                                    }
                                    break;
                                case "LiveSqlserver":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        livesqlserver = reader.Value;
                                    }
                                    break;
                                default:
                                    break;

                            }
                        }
                    }
                }
            }
            catch { }
            if (livesqlserver == "" | livePassword == "" | liveuserName == "")
            {
                MessageBox.Show("Please enter the Live SQL server connection credientails. No clients will be displayed");
            }
            else
            {
                string clientsConnectionString = "Data Source=" + livesqlserver + ";Initial Catalog=System.Master;User ID=" + liveuserName + ";Password=" + livePassword + "";
                SqlConnection clientConn = new SqlConnection(clientsConnectionString);
                try
                {
                    clientConn.Open();
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message);
                    //this.Close();
                }
                if (clientConn.State != ConnectionState.Open)
                {
                    MessageBox.Show("Falied to connect to System.Master database. Please check the request handler service");
                    //this.Close();
                }
                else
                {

                    SqlCommand sqlClientComm = new SqlCommand();
                    sqlClientComm.Connection = clientConn;
                    sqlClientComm.CommandText = "Select llocksys as selected ,*, llocksys as HasTestEnvironment from Clients";
                    SqlDataAdapter clientAdapter = new SqlDataAdapter();
                    clientAdapter.SelectCommand = sqlClientComm;
                    clientAdapter.Fill(ds);

                    //
                    try
                    {
                        string testClientsConnectionString = "Data Source=" + sqlserver + ";Initial Catalog=System.Master;User ID=" + userName + ";Password=" + password + "";
                        SqlConnection testClientConn = new SqlConnection(testClientsConnectionString);
                        sqlClientComm.CommandText = "Select CCLIENTID from Clients";
                        sqlClientComm.Connection = testClientConn;
                        testClientConn.Open();
                        SqlDataReader testClients = sqlClientComm.ExecuteReader();
                        if (testClients.FieldCount >0)
                        {
                            while (testClients.Read())
                            {
                               
                                foreach (DataRow client in ds.Tables[0].Rows)
                                {
                                    if (client["CCLIENTID"].ToString() == testClients.GetString(0).ToString())
                                    {
                                        client["HasTestEnvironment"] = true;
                                        break;
                                    }
                                }
                                                            }
                        }
                    }
                    catch(Exception y)
                    { }
                    //

                    clientList.DataSource = ds.Tables[0].DefaultView;
                    clientList.Columns[clientList.Columns.Count - 1].Visible = false;
                    for (int gridlist = 1; gridlist < clientList.Columns.Count; gridlist++)
                    {
                        clientList.Columns[gridlist].ReadOnly = true;
                    }
                    clientList.AllowUserToAddRows = false;
                    clientList.AllowUserToDeleteRows = false;
                    for (int n = 0; n < (clientList.Rows.Count); n++)
                    {
                        if ((bool)clientList.Rows[n].Cells["HasTestEnvironment"].Value == true)
                        {
                            for (int m = 0; m < (clientList.Columns.Count - 1); m++)
                            {
                                clientList.Rows[n].Cells[m].Style.BackColor = Color.GreenYellow  ;
                            }
                        }
                    }

                }
            }
 
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void btnOK_Click(object sender, EventArgs e)
        {
            if (btnOK.Text.ToUpper() == "BACK")
            {
                clientList.DataSource = ds.Tables[0].DefaultView;
                for (int gridlist = 1; gridlist < clientList.Columns.Count; gridlist++)
                {
                    clientList.Columns[gridlist].ReadOnly = true;
                }
                clientList.Columns[clientList.Columns.Count - 1].Visible = false;
                clientList.AllowUserToAddRows = false;
                clientList.AllowUserToDeleteRows = false;
                for (int n = 0; n < (clientList.Rows.Count); n++)
                {
                    if ((bool)clientList.Rows[n].Cells["HasTestEnvironment"].Value == true)
                    {
                        for (int m = 0; m < (clientList.Columns.Count - 1); m++)
                        {
                            clientList.Rows[n].Cells[m].Style.BackColor = Color.GreenYellow;
                        }
                    }
                }
                lblSelect.Text = lblSelect.Text.Replace("company", "client");
                btnOK.Text = "Next";
                btnFinish.Enabled = false;
                //lblcopyfile.Visible = false;
                //prgbcopying.Visible = false;
                return;
            }
            dsComp.Clear();
            bool llselect = false;
            foreach (DataRow row in ds.Tables[0].Rows)
            {
                if (bool.Parse(row["selected"].ToString()) != true)
                {
                    continue;
                }else
                {
                    llselect = true;
                }
                if (dsComp.Tables.Count == 0)
                {
                    dsComp.Tables.Add(GetClientCompaines(row["CCLIENTID"].ToString(), row["CCLIENTNAME"].ToString(), row["ARIA40SYS"].ToString()));
                }
                else
                {
                    dsComp.Tables[0].Merge(GetClientCompaines(row["CCLIENTID"].ToString(), row["CCLIENTNAME"].ToString(), row["ARIA40SYS"].ToString()));
                }
                  
            }
            if (!llselect || dsComp.Tables[0].Rows.Count == 0)
            {
                MessageBox.Show("There is no companies to show");
                clientList.DataSource = ds.Tables[0].DefaultView;
                for (int gridlist = 1; gridlist < clientList.Columns.Count; gridlist++)
                {
                        clientList.Columns[gridlist].ReadOnly = true;
                    }
                    clientList.AllowUserToAddRows = false;
                    clientList.AllowUserToDeleteRows = false;
                    lblSelect.Text = lblSelect.Text.Replace("company", "client");
                    btnOK.Text = "Next";
                    btnFinish.Enabled = false;
                    //lblcopyfile.Visible = false;
                    //prgbcopying.Visible = false;
                    return;
                }

            else {
                clientList.DataSource = dsComp.Tables[0].DefaultView;
                lblSelect.Text = lblSelect.Text.Replace("client", "company");
                btnOK.Text = "Back";
                for (int gridlist = 1; gridlist < clientList.Columns.Count; gridlist++)
                {
                    clientList.Columns[gridlist].ReadOnly = true;
                }
                clientList.AllowUserToAddRows = false;
                clientList.AllowUserToDeleteRows = false;
                btnFinish.Enabled = true; 
            }
            
        }
        public DataTable GetClientCompaines(string clientID,string Clientname, string clientSysFiles)
        {
            DataTable compTable = new DataTable();
            string connSrc = @"Provider=VFPOLEDB.1;Data Source=" + clientSysFiles;
            var cmdSrc = new OleDbCommand();
            var dbconnSrc = new OleDbConnection(connSrc);
            cmdSrc.Connection = dbconnSrc;
            cmdSrc.CommandText = "Select .f. as 'Selected','" + clientID + "' as 'Client ID','" + Clientname + "' as 'Client Name',* from SYCCOMP";
            try
            {
                dbconnSrc.Open();
                OleDbDataAdapter oleAdapterFox = new OleDbDataAdapter ();
                oleAdapterFox.SelectCommand =cmdSrc ;
                oleAdapterFox.Fill(compTable );
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
            return compTable;
        }

        private void btnFinish_Click(object sender, EventArgs e)
        {
            string password = "", destinationFolder = "", sqlserver = "", userName = "", dBFolder = "", backupFolder = "", livesqlserver = "", liveuserName = "", livePassword = "";
            try
            {
                XmlReader reader = XmlReader.Create(System.Windows.Forms.Application.StartupPath + @"\CreateTestingEnvironmentsettings.xml");
                while (reader.Read())
                {
                    if (reader.NodeType == XmlNodeType.Element && reader.Name == "settings")
                    {
                        while (reader.NodeType != XmlNodeType.EndElement)
                        {
                            reader.Read();
                            switch ((reader.Name))
                            {
                                case "DestinationFolder":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        destinationFolder = reader.Value;
                                    }
                                    break;
                                case "DataBaseFolder":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        dBFolder = reader.Value;
                                    }
                                    break;
                                case "BackupFolder":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        backupFolder = reader.Value;
                                    }
                                    break;
                                case "Sqlserver":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        sqlserver = reader.Value;
                                    }
                                    break;
                                case "UserName":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        userName = reader.Value;
                                    }
                                    break;
                                case "Password":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        password = reader.Value;
                                    }
                                    break;
                                case "LivePassword":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        livePassword = reader.Value;
                                    }
                                    break;
                                case "LiveUserName":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        liveuserName = reader.Value;
                                    }
                                    break;
                                case "LiveSqlserver":
                                    reader.Read();
                                    if (reader.NodeType == XmlNodeType.Text)
                                    {
                                        livesqlserver = reader.Value;
                                    }
                                    break;
                                default:
                                    break;

                            }
                        }
                    }
                }
            }
            catch { }
            string connetionString = null;
            SqlConnection cnn;
            connetionString = "Data Source=" + sqlserver + ";Initial Catalog=Master;User ID=" + userName + ";Password=" + password + "";
            cnn = new SqlConnection(connetionString);
            try
            {
                cnn.Open();
                cnn.Close();
            }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to connect to the SQL server!," + ex.Message);
                return;
            }
            if (!Directory.Exists(destinationFolder))
            {
                MessageBox.Show("Invalid Destination Directory");
                return;
            }
            if (!Directory.Exists(backupFolder))
            {
                MessageBox.Show("Invalid SQL Backup Directory");
                return;
            }
            if (!Directory.Exists(dBFolder))
            {
                MessageBox.Show("Invalid test SQL Database Directory");
                return;
            }


           // Aria.Utilities.RemoteCall.AriaActivator loActivator = new Aria.Utilities.RemoteCall.AriaActivator ();
           // Aria.Environment.AriaEnviromentVariables ariaEnv = (Aria.Environment.AriaEnviromentVariables)loActivator.GetRemoteObject("Aria.Environment.AriaEnviromentVariables", Environment.MachineName, 1500);
           // string clientsConnectionString = ariaEnv.Aria50SystemFilesConnectionString;
            string clientsConnectionString = @"Data Source=" + sqlserver + "; Initial Catalog=" + "System.Master" + "; User Id=" + userName + "; Password=" + password + ";"; 
            SqlConnection clientConn = new SqlConnection(clientsConnectionString);
            SqlCommand sqlClientComm = new SqlCommand();
            sqlClientComm.Connection = clientConn;
            SqlDataAdapter clientAdapter = new SqlDataAdapter();
            try
            {
                clientConn.Open();

            }
            catch (Exception ec)
            {
                MessageBox.Show(ec.Message);
            }
            
           

            ServerConnection con = new ServerConnection(sqlserver);
            con.ConnectionString = @"Data Source=" + sqlserver + "; Initial Catalog=" + "Master" + "; User Id=" + userName + "; Password=" + password + ";";
            con.Connect();
            Server server = new Server(con);
            SqlConnection con2 = new SqlConnection(con.ConnectionString);
            con2.Open();
            string copiedClients = "";
            foreach (DataRow row in dsComp.Tables[0].Rows)
            {
                string clientFolder = "";
                if (bool.Parse(row["selected"].ToString()) != true)
                {
                    continue;
                }
                else
                {
                    if (!copiedClients.Contains(row["Client ID"].ToString()))
                    {
                        string clientMasterDatabase = "", clientMasterServer = "", clientReqServer="", clientMasterUser = "", clientMasterPw = "", clientName = "", cinsdfComVal = "";
                        foreach (DataRow  clRow in ds.Tables[0].Rows)
                        {
                            if (clRow["CCLIENTID"].ToString() == row["Client ID"].ToString())
                            {
                                
                                clientFolder = clRow["CDATAPATH"].ToString ();
                                clientMasterDatabase = clRow["CCONDBNAME"].ToString();
                                clientMasterServer = clRow["CCONSERVER"].ToString();
                                clientMasterUser = clRow["CCONUSERID"].ToString();
                                clientMasterPw = clRow["CCONPASWRD"].ToString();
                                clientName = clRow["CCLIENTNAME"].ToString();
                                cinsdfComVal = clRow["CINSDFCOM"].ToString();
                                clientReqServer = clRow["REQSERVER"].ToString();
                                //if (!MapDrive("X", clientFolder.TrimEnd(), null, null))
                                //{
                                //    MessageBox.Show("Couldn't map client:" + clRow["CCLIENTID"].ToString()+" folder as X,Please Map it before pressing OK.");
                                //}
                                //
                            }
                        }
                        copiedClients += row["Client ID"].ToString()+",";
                        prgbcopying.Value = 0;
                        lblcopyfile.Text = "Copying client: "+row["Client ID"].ToString()+" Folder";
                        //lblcopyfile.Update();
                        prgbcopying.Increment(20);
                        Application.DoEvents();
                        CopyClientFolder(row["Client ID"].ToString(), destinationFolder, clientFolder);
                        
                        FileInfo bakFileMaset = new FileInfo(backupFolder + @"\" + clientMasterDatabase.TrimEnd() + ".bak");
                        try
                        {
                            bakFileMaset.Delete();
                        }
                        catch { }
                        
                        lblcopyfile.Text = "Copying client: " + row["Client ID"].ToString() + " Master Database";
                        prgbcopying.Increment(40);
                        lblcopyfile.Invalidate();
                        lblcopyfile.Update();
                        lblcopyfile.Refresh();
                        Application.DoEvents();

                        BackupDatabase(backupFolder + @"\" + clientMasterDatabase.TrimEnd() + ".bak", clientMasterServer.ToString(), clientMasterUser.ToString(), clientMasterPw.ToString(), clientMasterDatabase.TrimEnd ().ToString());
                        Directory.CreateDirectory(dBFolder + @"\" + row["Client ID"].ToString().TrimEnd());
                        bakFileMaset.CopyTo(Path.Combine(dBFolder + @"\" + row["Client ID"].ToString().TrimEnd(), bakFileMaset.Name), true);
                        SqlRestore(server, dBFolder + @"\" + row["Client ID"].ToString().TrimEnd() + @"\", dBFolder + @"\" + row["Client ID"].ToString().TrimEnd() + @"\", clientMasterDatabase.TrimEnd(), clientMasterDatabase.TrimEnd() + ".bak", clientMasterDatabase + "_LOG.LDF", false, con2, clientMasterDatabase);
                        //sqlClientComm.CommandText = "Select * From [System.Master].[dbo].[CLIENTS] Where CCLIENTID='" + row["Client ID"].ToString().TrimEnd() + "'";
                        //int clientFound = sqlClientComm.ExecuteNonQuery();
                        //if (clientFound != 0)
                        //{
                        sqlClientComm.CommandText = "Delete from [System.Master].[dbo].[CLIENTS] Where CCLIENTID='" + row["Client ID"].ToString().TrimEnd() + "'";
                        int DeleteRes = sqlClientComm.ExecuteNonQuery();
                        //}
                        sqlClientComm.CommandText = String.Format("INSERT INTO [System.Master].[dbo].[CLIENTS] ([CCLIENTID],[CCLIENTNAME]" +
                                           ",[CDATAPATH],[LLOCKSYS],[CINSDFCOM],[CCONSERVER],[CCONDBNAME],[CCONUSERID]" +
                                           ",[CCONPASWRD],[ARIA27SYS],[ARIA40SYS],[CTDATAPATH],[REQSERVER]) VALUES" +
                                           "('{0}','{1}','{2}',0,'{3}','{4}','{5}','{6}','{7}','{8}','{9}','','{10}')",
                                           row["Client ID"].ToString().TrimEnd(),
                                           clientName,
                                           destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T",
                                           cinsdfComVal, sqlserver, clientMasterDatabase.TrimEnd(),
                                           userName, password,
                                           destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\SYSFILES\",
                                           destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\SYSFILES\", Environment.MachineName.ToString());
                        int insertRes = sqlClientComm.ExecuteNonQuery();
                        ////
                        //
                        StringBuilder newFile = new StringBuilder();
                        string temp = "";
                        string[] file = System.IO.File.ReadAllLines(destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\SYSFILES\Client_Setting.xml");
                        foreach (string line in file)
                        {
                            if (line.Contains("<RemoteSrv>" + clientReqServer.TrimEnd () + "</RemoteSrv>"))
                            {

                                temp = line.Replace("<RemoteSrv>" + clientReqServer.TrimEnd() + "</RemoteSrv>", "<RemoteSrv>" + Environment.MachineName.ToString() + "</RemoteSrv>");
                                newFile.Append(temp + "\r\n");
                                continue;
                            }
                            newFile.Append(line + "\r\n");
                        }
                        System.IO.File.WriteAllText(destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\SYSFILES\Client_Setting.xml", newFile.ToString());
                        string connSrc = @"Provider=VFPOLEDB.1;Data Source=" + destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\SYSFILES\";
                        var cmdSrc = new OleDbCommand();
                        var dbconnSrc = new OleDbConnection(connSrc);
                        cmdSrc.Connection = dbconnSrc;
                        cmdSrc.CommandText = "Update SYCCOMP set ccom_ddir = '"+destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\DBFS\"+"'+ccomp_id";
                        try
                        {
                            dbconnSrc.Open();
                            cmdSrc.ExecuteNonQuery ();
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(ex.Message);
                        }
                        ////
                    }
                    lblcopyfile.Text = "Copying client: " + row["Client ID"].ToString() + " Companies";
                    prgbcopying.Increment(80);
                    lblcopyfile.Invalidate();
                    lblcopyfile.Update();
                    lblcopyfile.Refresh();
                    Application.DoEvents();


                    string dataDir = row["ccom_ddir"].ToString();
                    if (dataDir.ToUpper().Contains("X:"))
                    {
                        dataDir = dataDir.ToUpper().Replace(@"X:\", clientFolder.Trim());
                    }
                    Copy(dataDir, destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\DBFS\" + row["ccomp_id"].ToString() + @"\");
                    //Delete the backup file if exist
                    FileInfo bakFile = new FileInfo(backupFolder + @"\" + row["ccondbname"].ToString().TrimEnd() + ".bak");
                    try
                    {
                        bakFile.Delete();
                    }
                    catch { }
                    //
                    if (!Directory.Exists ((dBFolder + @"\" + row["Client ID"].ToString().TrimEnd())))
                    {
                       Directory.CreateDirectory(dBFolder + @"\" + row["Client ID"].ToString().TrimEnd());
                    }
                    BackupDatabase(backupFolder + @"\" + row["ccondbname"].ToString().TrimEnd() + ".bak", row["cconserver"].ToString(), row["cconuserid"].ToString(), row["cconpaswrd"].ToString(), row["ccondbname"].ToString());

                    // copy the backup file from source to the destination database path
                    //FileInfo bakFile =  new FileInfo (backupFolder + @"\" + row["ccondbname"].ToString().TrimEnd() + ".bak");
                    bakFile.CopyTo(Path.Combine(dBFolder + @"\" + row["Client ID"].ToString().TrimEnd(), bakFile.Name), true);
                    //
                    SqlRestore(server, dBFolder + @"\" + row["Client ID"].ToString().TrimEnd() + @"\", dBFolder + @"\" + row["Client ID"].ToString().TrimEnd() + @"\", row["ccondbname"].ToString().TrimEnd().ToUpper().Replace("LDB", "TDB"), row["ccondbname"].ToString().TrimEnd() + ".bak", row["ccondbname"].ToString().TrimEnd().ToUpper().Replace("LDB", "TDB") + "_LOG.LDF", false, con2, row["ccondbname"].ToString().TrimEnd());
                    string connSrcComp = @"Provider=VFPOLEDB.1;Data Source=" + destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\SYSFILES\";
                    var cmdSrcComp = new OleDbCommand();
                    var cmdSrcCompConn = new OleDbConnection(connSrcComp);
                    cmdSrcComp.Connection = cmdSrcCompConn;
                    cmdSrcComp.CommandText = "Update SYCCOMP set "+
                    "ccom_ddir = '"+destinationFolder + @"\" + row["Client ID"].ToString().TrimEnd() + "SH-T" + @"\Aria4XP\DBFS\"+"'+ccomp_id,"+
                    "cconserver = '" + sqlserver + "',cconuserid='"+userName +"',"+
                    "cconpaswrd='"+password+"',ccondbname='"+
                    row["ccondbname"].ToString().TrimEnd().ToUpper().Replace("LDB", "TDB") +"',"+
                    "ccom_ddir='" + destinationFolder + @"\" +row["Client ID"].ToString().TrimEnd() + @"SH-T\Aria4XP\DBFS\'+ccomp_id+'\' "+"where ccomp_id ='" + row["ccomp_id"].ToString().TrimEnd()+ "'";
                    try
                    {
                        cmdSrcCompConn.Open();
                        cmdSrcComp.ExecuteNonQuery();
                    }
                    catch (Exception ex)
                    {
                        MessageBox.Show(ex.Message);
                    }
                    lblcopyfile.Text = "Copying client: " + row["Client ID"].ToString() + " Companies";
                    prgbcopying.Increment(100);
                    lblcopyfile.Invalidate();
                    lblcopyfile.Update();
                    lblcopyfile.Refresh();
                    Application.DoEvents();
                 }
            }
            this.Close();
        }

        private void btnSetting_Click(object sender, EventArgs e)
        {
            Settings settings = new Settings();
            settings.ShowDialog();
        }
        public bool CopyClientFolder(String clientID, string destinationFolder, string sourceFolder)
        {
            if (!Directory.Exists(destinationFolder))
            {
                MessageBox.Show("Invalid Destination Directory:" + destinationFolder);
                return false;
            }
            if (!Directory.Exists(sourceFolder))
            {
                MessageBox.Show("Invalid source Directory:" + sourceFolder);
                return false;
            }
            Directory.CreateDirectory(destinationFolder + @"\" + clientID.TrimEnd ()+"SH-T");
            //destinationFolder.CreateSubdirectory(clientID);
            Copy(sourceFolder, destinationFolder + @"\" + clientID.TrimEnd ()+"SH-T");
            return true;
        }
        public static void Copy(string sourceDirectory, string targetDirectory)
        {
            DirectoryInfo diSource = new DirectoryInfo(sourceDirectory);
            DirectoryInfo diTarget = new DirectoryInfo(targetDirectory);
            
            CopyAll(diSource, diTarget);
        }

        public static void CopyAll(DirectoryInfo source, DirectoryInfo target)
        {

            // Check if the target directory exists; if not, create it.
            if (Directory.Exists(target.FullName) == false)
            {
                Directory.CreateDirectory(target.FullName);
            }

            // Copy each file into the new directory.
            foreach (FileInfo fi in source.GetFiles())
            {
                //Console.WriteLine(@"Copying {0}\{1}", target.FullName, fi.Name);
                try
                {
                    fi.CopyTo(Path.Combine(target.ToString(), fi.Name), true);
                }
                catch(Exception e)
                {
                    MessageBox.Show(e.Message);
                }
            }

            // Copy each subdirectory using recursion.
            foreach (DirectoryInfo diSourceSubDir in source.GetDirectories())
            {
                DirectoryInfo nextTargetSubDir =
                    target.CreateSubdirectory(diSourceSubDir.Name);
                if (diSourceSubDir.Name.ToUpper ()!="DBFS")
                {
                  CopyAll(diSourceSubDir, nextTargetSubDir);
                }
            }
        }
        public static void BackupDatabase(string backUpFile,string sqlSever,string userName,string password,string databaseName)
            {
             ServerConnection con = new ServerConnection(sqlSever);
                //con.ConnectionString = @"Data Source=" + sqlSever + ";Initial Catalog=" + databaseName + ";Integrated Security=True;User Id=" + userName + ";Password=" + password + ";";
            //con.ConnectionString = @"Data Source=.; Initial Catalog=99mmt; User Id=sa; Password=mmt123;";
                //con.ConnectionString = @"Server=" + sqlSever + ";Database=" + databaseName + ";User ID=" + userName + ";Password=" + password + ";Trusted_Connection=False";
                con.ConnectionString = @"Data Source=" + sqlSever + "; Initial Catalog=" + databaseName + "; User Id=" + userName + "; Password=" + password + ";";
                            con.Connect();
            Server server = new Server(con);
            Backup source = new Backup();
            source.Action = BackupActionType.Database;
            source.Database = databaseName;
            source.Incremental =false;
            BackupDeviceItem destination = new BackupDeviceItem(backUpFile, DeviceType.File);
            source.Devices.Add(destination);
            try
            {
            source.SqlBackup(server);
            }
            catch (Exception ex)
            {
              MessageBox.Show(ex.InnerException.InnerException.Message);
            }
            con.Disconnect();
            }
        public static void RestoreDatabase(string backUpFile, string sqlSever, string userName, string password, string databaseName)
        {
            //try
            //{
                ServerConnection con = new ServerConnection(sqlSever);
                con.ConnectionString = @"Data Source=" + sqlSever + "; Initial Catalog=" + "Master" + "; User Id=" + userName + "; Password=" + password + ";";
                con.Connect();
                Server server = new Server(con);
                Database db = server.Databases["Master"];
                string dbPath = Path.Combine(db.PrimaryFilePath, databaseName+".mdf");
                string logPath = Path.Combine(db.PrimaryFilePath, databaseName+"_Log.ldf");
                Restore restore = new Restore();
                BackupDeviceItem deviceItem =     new BackupDeviceItem(backUpFile, DeviceType.File);
                restore.Devices.Add(deviceItem);
                restore.Database = databaseName;
                restore.FileNumber = 1;
                restore.Action = RestoreActionType.Database;
                restore.ReplaceDatabase = true;
                restore.SqlRestore(server);
                db = server.Databases[databaseName];
                db.SetOnline();
                server.Refresh();
                db.Refresh();
            }
        private string PrepareCommand(string Name, string DatabaseName, string dataPath, string backUpFile)
        {
            string Commands = this.LoadSQLFromAssembly(Name);

            if (Commands == null)
                return null;

            Commands = Commands.Replace("%Database%", DatabaseName);

            Commands = Commands.Replace("%DatabasePath%", dataPath + "\\");

            Commands = Commands.Replace("%BackUpPath%", backUpFile);

            return Commands;

        }
        private string LoadSQLFromAssembly(string Name)
        {
            System.IO.Stream stream = this.GetType().Assembly.GetManifestResourceStream(this.GetType(), "SQL." + Name);

            if (stream == null)
            {
                MessageBox.Show("Internal Error occured! Close Application & try again.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);

                return null;
            }

            System.IO.StreamReader reader = new System.IO.StreamReader(stream);

            if (reader == null)
            {
                MessageBox.Show("Internal Error occured! Close Application & try again.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);

                return null;
            }

            string s = reader.ReadToEnd();

            reader.Close();

            return s;

        }
        private bool ExecuteSQLCommand(string ConnectionString, string Commands)
        {

            SqlConnection connection = new SqlConnection(ConnectionString);
            SqlCommand cmd = connection.CreateCommand();

            cmd.CommandText = Commands;

            try
            {

                connection.Open();

                cmd.ExecuteNonQuery();

            }
            catch (SqlException ex)
            {
                MessageBox.Show(ex.ToString(), "SQL Server Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);

                return false;
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);

                return false;
            }
            finally
            {
                if (connection.State == ConnectionState.Open)
                    connection.Close();

                cmd.Dispose();

                connection.Dispose();

            }

            return true;
        }
        private void DoRestore(string backUpFile, string sqlSever, string userName, string password, string databaseName)
        {
            string Commands, DataFile = "", LogFile = "", DatabaseName = "";
            Commands = this.PrepareCommand("CreateNewDatabase.sql", databaseName,@"D:\",backUpFile);
            if (Commands == null)                {    return; }

                if (this.ExecuteSQLCommand(@"Data Source=" + sqlSever + "; Initial Catalog=" + "Master" + "; User Id=" + userName + "; Password=" + password + ";", Commands))
                {
                 
                }
                else
                {
                    return;
                } 
                 if (this.GetFileRestored(@"Data Source=" + sqlSever + "; Initial Catalog=" + "Master" + "; User Id=" + userName + "; Password=" + password + ";", DatabaseName, ref DataFile, ref LogFile,@"D:\",backUpFile))
                {
                Commands = this.PrepareCommand("RestoreFinall.sql", databaseName,@"D:\",backUpFile);
                if (Commands == null)
                {
                  return;
                }

                Commands = Commands.Replace("%OldData%", DataFile).Replace("%OldLog%", LogFile);


                this.ExecuteSQLCommand(@"Data Source=" + sqlSever + "; Initial Catalog=" + "Master" + "; User Id=" + userName + "; Password=" + password + ";", Commands);

                              
            }

            

            

        }

        private bool GetFileRestored(string ConnectionString, string DatabaseName, ref string DataFile, ref string LogFile, string dataPath, string backUpFile)
		{
			SqlConnection	connection	= new SqlConnection(ConnectionString);
			SqlCommand		cmd			= connection.CreateCommand();
			SqlDataReader	reader;
			
			cmd.CommandText = this.PrepareCommand("Restore.sql",DatabaseName,  dataPath, backUpFile);
			try
			{

				connection.Open();
				reader = cmd.ExecuteReader();
				while(reader.Read())
				{
					if(reader.GetString(2)=="D")
						DataFile = reader.GetString(0);
					else
						LogFile = reader.GetString(0);
				}

				reader.Close();

				(reader as IDisposable).Dispose();

			}
			catch(SqlException ex)
			{
				MessageBox.Show(ex.ToString(),"SQL Server Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);

				return false;
			}
			catch(Exception ex)
			{
				MessageBox.Show(ex.ToString(),"Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);

				return false;
			}
			finally
			{
				if(connection.State == ConnectionState.Open)
					connection.Close();
			
				cmd.Dispose();

				connection.Dispose();

			}
			return true;
		}

        public static void SqlRestore(
            // Sql Server name.
        Server sqlServer,
            // The backup file directory.
        string backupFilePath,
            // The database directory.
        string databaseFolder,
            // The desired restored database name.
        string destinationDatabaseName,
            // The backup filename.
        string backupFilename,
            // The database log name.
        string databaseLogFileName,
            // This will restore the database in partial mode for more info about partial check in the stackoverflow
        bool partial, SqlConnection tmpConn,string orgDbName)
        {
            
            //Define a Backup object variable.
            var sqlRestore = new Restore
            {
                Database = destinationDatabaseName,
                NoRecovery = false,
                ReplaceDatabase = true,

                // Specify the type of backup, the description, the name, and the database to be backed up.
                Action = RestoreActionType.Database
            };

            //var sqlServer = GetSqlServer(serverName);

            // If the backup directory not found then get the default.
            if (string.IsNullOrEmpty(databaseFolder))
            {
                const string SqlDataDirectory = "DATA";
                databaseFolder = sqlServer.InstallDataDirectory + Path.DirectorySeparatorChar + SqlDataDirectory;
            }

            // Declare a BackupDeviceItem
            var deviceItem = new BackupDeviceItem(
              backupFilePath + Path.DirectorySeparatorChar + backupFilename,
              DeviceType.File);

            const string SqlMdfExtention = ".mdf";
            const string SqlLdfLogExtention = "_log.ldf";

            var dataFileLocation = databaseFolder + Path.DirectorySeparatorChar + destinationDatabaseName.TrimEnd () + SqlMdfExtention;
            var logFileLocation = databaseFolder + Path.DirectorySeparatorChar + destinationDatabaseName.TrimEnd() + SqlLdfLogExtention;
            
           

            sqlRestore.ReplaceDatabase = true;
            sqlRestore.ContinueAfterError = false;

            sqlRestore.PercentCompleteNotification = 1;
           

            if (partial)
            {
                sqlRestore.Partial = true;
                sqlRestore.ContinueAfterError = true;
            }

            //sqlServer.KillAllProcesses(destinationDatabaseName);
            sqlRestore.Database = destinationDatabaseName.TrimEnd();
            sqlServer.ConnectionContext.StatementTimeout = 60 * 60;

            // Add the device to the Restore object.
            sqlRestore.Devices.Add(deviceItem);
            DataTable dt = sqlRestore.ReadFileList(sqlServer);
            string realLogName = "", realDbName = "";
            for (int i = 0; i < dt.Rows.Count; i++)
            {
                if (dt.Rows[i]["Type"].ToString() == "L")
                {
                    realLogName = dt.Rows[i]["LogicalName"].ToString();
                    
                }
                else if (dt.Rows[i]["Type"].ToString() == "D")
                {
                    realDbName = dt.Rows[i]["LogicalName"].ToString();
               }
            }
            //sqlRestore.RelocateFiles.Add(new RelocateFile(orgDbName.TrimEnd(), dataFileLocation));
            //sqlRestore.RelocateFiles.Add(new RelocateFile(orgDbName.TrimEnd() + "_log", logFileLocation));
            sqlRestore.RelocateFiles.Add(new RelocateFile(realDbName, dataFileLocation));
            sqlRestore.RelocateFiles.Add(new RelocateFile(realLogName, logFileLocation));
            // Run SqlRestore to perform the database restore on the instance of SQL Server.
            try
            {
                sqlRestore.SqlRestore(sqlServer);
                var database = sqlServer.Databases[destinationDatabaseName];
                database.SetOnline();

                // Remove the restore device from the restore object.
                sqlRestore.Devices.Remove(deviceItem);

                // Disconnect and dispose the connection.
                sqlServer.ConnectionContext.Disconnect();
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.InnerException.InnerException.Message);
            }

           
        }

        
        
            
        

        private void MapNetDrive(string driveChar, string server, string user, string password)
        {
            try
            { 



                IWshNetwork_Class network = new IWshNetwork_Class();
                
                network.RemoveNetworkDrive(driveChar,true);
                network.MapNetworkDrive(driveChar, server);


            }
            catch (Exception e)
            {
                MessageBox.Show(e.ToString());
            }
        }
    
        ///
        /// public static class NetworkDrives

  public static bool MapDrive(string DriveLetter, string Path, string Username, string Password)
  {
 
    bool ReturnValue = false;
 
    if (System.IO.Directory.Exists(DriveLetter + ":\\"))
    {
      DisconnectDrive(DriveLetter);
    }
    NetworkDrive driveToMap = new NetworkDrive();
    driveToMap.Force = true;
    driveToMap.LocalDrive = DriveLetter + ":";
    driveToMap.ShareName = Path;
    driveToMap.MapDrive();
    if (System.IO.Directory.Exists(DriveLetter + ":\\"))
    {
        ReturnValue = true;
    }
    //System.Diagnostics.Process p = new System.Diagnostics.Process();
    //p.StartInfo.UseShellExecute = false;
    //p.StartInfo.CreateNoWindow = true;
    //p.StartInfo.RedirectStandardError = true;
    //p.StartInfo.RedirectStandardOutput = true;
 
    //p.StartInfo.FileName = "net.exe";
    //p.StartInfo.Arguments = " use " + DriveLetter + ": " + Path + " " + Password + " /user:" + Username;
    //p.Start();
    //p.WaitForExit();
 
    //string ErrorMessage = p.StandardError.ReadToEnd();
    //string OuputMessage = p.StandardOutput.ReadToEnd();
    //if (ErrorMessage.Length > 0)
    //{
    //  throw new Exception("Error:" + ErrorMessage);
    //}
    //else
    //{
    //  ReturnValue = true;
    //}
    return ReturnValue;
  }
    
public static bool DisconnectDrive(string DriveLetter)
  {
    bool ReturnValue = false;
    System.Diagnostics.Process p = new System.Diagnostics.Process();
    p.StartInfo.UseShellExecute = false;
    p.StartInfo.CreateNoWindow = true;
    p.StartInfo.RedirectStandardError = true;
    p.StartInfo.RedirectStandardOutput = true;
 
    p.StartInfo.FileName = "net.exe";
    p.StartInfo.Arguments = " use " + DriveLetter + ": /DELETE /yes";
    p.Start();
    p.WaitForExit();
 
    string ErrorMessage = p.StandardError.ReadToEnd();
    string OuputMessage = p.StandardOutput.ReadToEnd();
    if (ErrorMessage.Length > 0)
    {
      throw new Exception("Error:" + ErrorMessage);
    }
    else
    {
      ReturnValue = true;
    }
    return ReturnValue;
  }
 
        ///
    }
}
