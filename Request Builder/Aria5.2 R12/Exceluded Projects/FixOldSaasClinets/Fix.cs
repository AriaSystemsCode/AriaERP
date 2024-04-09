using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Data.SqlClient; 
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Data.Odbc;
using System.Diagnostics;
using Aria.HelperClass.SAAS;

namespace FixOldSaasClinets
{
    public partial class Fix : Form
    {  string _serverName       = "";
       string _dbServerUserName = "";
       string _dbServerPassword = "";
       string _sysA27folder     = "";
       string _sysA4folder      = "";
       string _curClient        = "";
       AriaLogManage LogM = new AriaLogManage();
        public Fix()
        {
            InitializeComponent();
        }
   
        private void chkwindowsauth_CheckedChanged(object sender, EventArgs e)
        {
            if (chkwindowsauth.Checked == true)
            {
                txtpassword.Enabled = false;
                txtusername.Enabled = false;

                txtpassword.Text = "";
                txtusername.Text = "";
            }
            else
            {
                txtpassword.Enabled = true;
                txtusername.Enabled = true;

                txtpassword.Text = "";
                txtusername.Text = "";
            };

            if (string.IsNullOrEmpty(txtsqlserver.Text.ToString()) == false && chkwindowsauth.Checked)
            {
                cmdfixxml.Enabled = true;
            }
            else
            {
                cmdfixxml.Enabled = false;
            };
                
        }

        private void cmdfixxml_Click(object sender, EventArgs e)
        {
            #region Open Database connection and get clients data
            SqlConnection sqlConnection;
            if (chkwindowsauth.Checked == true)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + txtsqlserver.Text.ToString() + @";Initial Catalog=SYSTEM.Master;Trusted_Connection=yes");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + txtsqlserver.Text.ToString() + @";Initial Catalog=SYSTEM.Master;User ID=" + txtusername.Text.ToString() + @";Password=" + txtpassword.Text.ToString() + @";Trusted_Connection=no");
            };


            SqlCommand cmd = new SqlCommand("SELECT * FROM CLIENTS order by CCLIENTID");
            cmd.Connection = sqlConnection;
            sqlConnection.Open();

            DataTable dt = new DataTable();
            SqlDataAdapter sqldtadp = new SqlDataAdapter();
            sqldtadp.SelectCommand = cmd;
            sqldtadp.Fill(dt);
            #endregion 

            // loop on clients data 
            for (int i = 1; i <= dt.Rows.Count ; i++)
            {
                string ClientID = dt.Rows[i - 1]["CCLIENTID"].ToString().TrimEnd();
                string ClientSharedFolder = dt.Rows[i - 1]["CDATAPATH"].ToString().TrimEnd();

                _sysA27folder = dt.Rows[i - 1]["ARIA27SYS"].ToString().TrimEnd();
                _sysA4folder  = dt.Rows[i - 1]["ARIA40SYS"].ToString().TrimEnd();
                _curClient    = dt.Rows[i - 1]["CCLIENTID"].ToString().TrimEnd();

                #region UPDATE THE LBLPROGRESS LABEL
                lblProgress.Text = "Fix Clinet: "+ dt.Rows[i - 1]["CCLIENTNAME"].ToString().TrimEnd()  +" XML file and Client level DB ...";
                this.Refresh(); 
                #endregion 

                # region Check if client xml not exist add it, and then attach client master DB and update client record
                if (ClientXML(ClientSharedFolder + @"\ARIA4XP\SQLDictionary\Client_Setting.XML") == true )
                {
                    System.IO.File.Delete(ClientSharedFolder + @"\ARIA4XP\SQLDictionary\Client_Setting.XML");
                }

                if (ClientXML(ClientSharedFolder + @"\ARIA4XP\Client_Setting.XML") == false)
                {
                    AddClientXML(ClientSharedFolder + @"\ARIA4XP\Client_Setting.XML", ClientID);

                };

                #region  Attach\Detach client Master DB
                String DBName = ClientID + @".Master";
                String DBFullpath = @"D:\SQL_DATABASES\" + ClientID + @"\" + ClientID + @".Master.mdf";
                if (System.IO.File.Exists(DBFullpath)== false)
                {
                    System.IO.File.Copy("Aria.Master.mdf", DBFullpath);
                    System.IO.File.Copy("Aria.Master_log.ldf", DBFullpath.ToLower().Replace(".mdf", "_log.ldf"));
                };

                AttachDataBase(DBFullpath, txtsqlserver.Text.ToString(), DBName, txtusername.Text.ToString(), txtpassword.Text.ToString());
                #endregion

                #region Update the client table record in system.master
                updateClientRecord(ClientID, txtsqlserver.Text.ToString(), DBName, txtusername.Text.ToString(), txtpassword.Text.ToString());
                #endregion 

                _serverName = txtsqlserver.Text.ToString();
                _dbServerUserName = txtusername.Text.ToString();
                _dbServerPassword = txtpassword.Text.ToString();
                LinkedServerUpdate();
                
                #endregion 

            }

            #region close the connection
            sqlConnection.Close();
            #endregion 

            MessageBox.Show(" Finisehd From Fixing " + dt.Rows.Count.ToString() + " Clients Folders",this.Text);

        }

        /// Used to Attach Database
        /// </summary>
        /// <param name="DataBaseFullPath">Path of database must attachec from it.</param>
        /// <param name="ServerName">Server name that wnat attached DB into.</param>
        /// <param name="DataBaseName">Database name</param>
        /// <param name="Username">USer that wnat to be attacheed this database.</param>
        /// <param name="Password">Password of this user.</param>
        private void AttachDataBase(string DataBaseFullPath, string ServerName, string DataBaseName, string Username, string Password)
        {
            SqlConnection sqlConnection;
            if (chkwindowsauth.Checked == true)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + ServerName + @";Initial Catalog=Master;Trusted_Connection=yes");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + ServerName + @";Initial Catalog=Master;User ID=" + Username + @";Password=" + Password + @";Trusted_Connection=no");
            };
            //[sp_detach_db]
            SqlCommand cmd = new SqlCommand("sp_detach_db", sqlConnection);
            
            sqlConnection.Open();

            cmd.CommandType = CommandType.StoredProcedure;
            cmd.Parameters.Add("@dbname", SqlDbType.NVarChar).Value = DataBaseName;
           
            cmd.ExecuteNonQuery();


            
            SqlCommand cmd2 = new SqlCommand("sp_attach_db", sqlConnection);
            //sqlConnection.Open();
            cmd2.CommandType = CommandType.StoredProcedure;
            cmd2.Parameters.Add("@dbname", SqlDbType.NVarChar).Value = DataBaseName;
            cmd2.Parameters.Add("@filename1", SqlDbType.NVarChar).Value = @DataBaseFullPath;
            cmd2.Parameters.Add("@filename2", SqlDbType.NVarChar).Value = @DataBaseFullPath.ToLower().Replace(".mdf", "_log.ldf");
            
            cmd2.ExecuteNonQuery();
            
            sqlConnection.Close();
        }
        /// <summary>
        /// Used to Update the client record at clients table in system master DB with the client DB connection info
        /// </summary>
        /// <param name="ClientID"></param>
        /// <param name="CCONSERVER"></param>
        /// <param name="CCONDBNAME"></param>
        /// <param name="CCONUSERID"></param>
        /// <param name="CCONPASWRD"></param>
        private void updateClientRecord(String ClientID, String CCONSERVER, String CCONDBNAME, String CCONUSERID, String CCONPASWRD)
        {
            string ClientInsert = @"UPDATE CLIENTS SET [ARIA27SYS]=RTRIM(CDATAPATH)+'ARIA27\SYSFILES\', [ARIA40SYS]=RTRIM(CDATAPATH)+'ARIA4XP\SQLDICTIONARY\', [CCONSERVER]= '" + CCONSERVER + "'" + ",[CCONDBNAME]= '" + CCONDBNAME + "'" + ",[CCONUSERID]= '" + CCONUSERID + "'" + ",[CCONPASWRD] =  '" + CCONPASWRD + "' WHERE [CCLIENTID] = '" + ClientID + "'";
            SqlConnection sqlConnection;
            if (chkwindowsauth.Checked == true)
            {
                sqlConnection = new SqlConnection(@"Data Source=" + txtsqlserver.Text.ToString() + @";Initial Catalog=SYSTEM.Master;Trusted_Connection=yes");
            }
            else
            {
                sqlConnection = new SqlConnection(@"Data Source=" + txtsqlserver.Text.ToString() + @";Initial Catalog=SYSTEM.Master;User ID=" + txtusername.Text.ToString() + @";Password=" + txtpassword.Text.ToString() + @";Trusted_Connection=no");
            };


            SqlCommand cmd = new SqlCommand(ClientInsert);
            cmd.Connection = sqlConnection;
            sqlConnection.Open();

            cmd.ExecuteNonQuery();
            sqlConnection.Close();
                    

        }
        /// <summary>
        /// Check if this client has XML file in his folder
        /// </summary>
        /// <param name="clientid"></param>
        /// <returns></returns>
        private Boolean ClientXML(string newXMLFile)
        {

            return System.IO.File.Exists(newXMLFile);

        }
        /// <summary>
        ///  Add XML to current Client File
        /// </summary>
        private void AddClientXML(string newXMLFile, string ClientID )
        {
            DataSet DS = new DataSet();
            DS.ReadXml("configuration settings.xml");
            DS.Tables[0].Rows[0]["ClientID"] = ClientID;
            DS.WriteXml(newXMLFile);
                
            //System.IO.File.Copy("configuration settings.xml", newXMLFile);
        }

        private void txtsqlserver_TextChanged(object sender, EventArgs e)
        {
            changefixbuttonstatus();

        }

        private void txtusername_TextChanged(object sender, EventArgs e)
        {
            changefixbuttonstatus();
        }

        private void txtpassword_TextChanged(object sender, EventArgs e)
        {
            changefixbuttonstatus();
        }

        private void changefixbuttonstatus()
        {
            if (chkwindowsauth.Checked)
            {
                cmdfixxml.Enabled = !(string.IsNullOrEmpty(txtsqlserver.Text.ToString()));
            }
            else
            {
                cmdfixxml.Enabled = !(string.IsNullOrEmpty(txtsqlserver.Text.ToString()) || string.IsNullOrEmpty(txtusername.Text.ToString()) || string.IsNullOrEmpty(txtpassword.Text.ToString()));
            };
        }


        public string LinkedServerUpdate()
        {
            try
            {
                string ConnectionString = @"Driver={Microsoft Visual FoxPro Driver};sourcedb=" + _sysA27folder + ";sourcetype=DBF;exclusive=No;Backgroundfetch=NO;collate=Machine;null=No;deleted=Yes";
                OdbcConnection Conn = new OdbcConnection(ConnectionString);
                Conn.Open();
                OdbcCommand cmd = new OdbcCommand("select * from SYCCOMP", Conn);
                DataTable DT = new DataTable();
                DT.Load(cmd.ExecuteReader());
                Conn.Close();
                for (int i = 0; i < DT.Rows.Count + 1; i++)
                {
                    string Arg = "";
                    if (!string.IsNullOrEmpty(DT.Rows[i]["cconuserid"].ToString()))
                    {
                        Arg = "-S " + DT.Rows[i]["cconserver"].ToString() + " -U " + DT.Rows[i]["cconuserid"].ToString() + " -P " + DT.Rows[i]["cconpaswrd"].ToString() + " -i \"" + Application.StartupPath + "\\Database Scripts\\script.txt\"";
                        if (i == DT.Rows.Count)
                            Arg = "-S " + _serverName + " -U " + _dbServerUserName + " -P " + _dbServerPassword + " -i \"" + Application.StartupPath + "\\Database Scripts\\script.txt\"";
                    }
                    else
                    {
                        Arg = "-S " + DT.Rows[i]["cconserver"].ToString() + " -i \"" + Application.StartupPath + "\\Database Scripts\\script.txt\"";
                        if (i == DT.Rows.Count)
                            Arg = "-S " + _serverName + " -i \"" + Application.StartupPath + "\\Database Scripts\\script.txt\"";
                    }
                    //int ExitCode;
                    Process Process1;

                    ProcessStartInfo ProcessInfo = new ProcessStartInfo("sqlcmd", Arg);
                    ProcessInfo.CreateNoWindow = true;
                    ProcessInfo.UseShellExecute = false;
                    ProcessInfo.RedirectStandardError = true;
                    ProcessInfo.RedirectStandardOutput = true;
                    ProcessInfo.ErrorDialog = true;

                    Process1 = Process.Start(ProcessInfo);
                    //ExitCode = Process1.ExitCode;
                    Process1.StartInfo.RedirectStandardError = true;
                    Process1.StartInfo.RedirectStandardOutput = true;
                    string error = Process1.StandardError.ReadToEnd();
                    string output = Process1.StandardOutput.ReadToEnd();
                    Process1.Close();

                    if (error.Length > 0)
                    {
                        LogM.AddLog("Error", "Fix Old Saas Clients - Register Aria4 Companies ", "Error for Update script: " + error.ToString(), _curClient);
                        return "Error for Update script: " + error;
                    }
                    return _curClient.ToString()+ " Success: " + output;
                }
            }
            catch (Exception Ex)
            {
                
                LogM.AddLog("Error", "Fix Old Saas Clients - Register Aria4 Companies ", "Exception: " + Ex.Message, _curClient);
                return _curClient+" " + Ex.Message;
            }
            return "";
        }

    }
}
