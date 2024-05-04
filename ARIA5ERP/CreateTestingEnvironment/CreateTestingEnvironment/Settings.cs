using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Data.Sql;
using System.Data.SqlClient;
using System.Xml;
using System.IO;

namespace CreateTestingEnvironment
{
    public partial class Settings : Form
    {
        public bool llTestConnection = false,llLiveConnection = false;

        public Settings()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            DialogResult result = folderBrowserDialog1.ShowDialog();
            if (result == DialogResult.OK)
            {
                txtPath.Text = folderBrowserDialog1.SelectedPath.ToString (); 
            }
        }

        private void Settings_Load(object sender, EventArgs e)
        {
            string myServer = Environment.MachineName;

            DataTable servers = SqlDataSourceEnumerator.Instance.GetDataSources();
            foreach (DataRow row in servers.Rows)
            {
                if ((row["InstanceName"] as string) != null)
                {
                    CmbServerName.Items.Add(row["ServerName"] + "\\" + row["InstanceName"]);
                    cmbLvServer.Items.Add(row["ServerName"] + "\\" + row["InstanceName"]);
                }
                else
                {
                    CmbServerName.Items.Add(row["ServerName"]);
                    cmbLvServer.Items.Add(row["ServerName"]);
                }

            }
            string password = "", destinationFolder = "", sqlserver = "", userName = "", dBFolder = "", backupFolder = "", livesqlserver = "", liveuserName = "",livePassword="";
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
            CmbServerName.Text = sqlserver;
            txtPassword.Text = password;
            txtPath.Text = destinationFolder;
            txtUserName.Text = userName;
            txtSQLDstPath.Text =dBFolder;
            txtSQLBackup.Text = backupFolder;
            txtLiveUser.Text = liveuserName;
            txtLivePW.Text = livePassword;
            cmbLvServer.Text = livesqlserver; 
        }

        private void btnTestConn_Click(object sender, EventArgs e)
        {
            string connetionString = null;
            SqlConnection cnn ;
            connetionString = "Data Source="+CmbServerName.Text+";Initial Catalog=Master;User ID="+txtUserName.Text+";Password="+txtPassword.Text+"";
            cnn = new SqlConnection(connetionString);
            try
            {
                cnn.Open();
                MessageBox.Show ("Connected successfully to the server");
                llTestConnection = true;
                cnn.Close();
            }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to connect to the server!,"+ex.Message);
                llTestConnection = false;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            if (txtPath.Text == "" | CmbServerName.Text == "" | txtUserName.Text == "" | txtPassword.Text == "" | txtSQLDstPath.Text == "" | txtSQLBackup.Text=="" | cmbLvServer.Text =="" | txtLivePW.Text =="" | txtLiveUser.Text =="")
            {
                MessageBox.Show("Invalid settings, cannot save!");
                return ;
            }
            if (!Directory.Exists(txtPath.Text))
            {
                MessageBox.Show("Invalid Destination Directory");
                return;
            }
            if (!llTestConnection)
            {
                MessageBox.Show("Please test the Testing SQL server connection");
                return;
            }
            if (!llLiveConnection)
            {
                MessageBox.Show("Please test the Live SQL server connection");
                return;
            }
            XmlWriterSettings settings = new XmlWriterSettings();
            settings.Indent = true;
            XmlWriter writer = XmlWriter.Create(System.Windows.Forms.Application.StartupPath + @"\CreateTestingEnvironmentsettings.xml", settings);
            writer.WriteStartDocument();
            writer.WriteStartElement("settings");
            writer.WriteElementString("LiveSqlserver", cmbLvServer.Text);
            writer.WriteElementString("LiveUserName", txtLiveUser.Text);
            writer.WriteElementString("LivePassword", txtLivePW.Text);
            writer.WriteElementString("DestinationFolder", txtPath.Text);
            writer.WriteElementString("Sqlserver", CmbServerName.Text);
            writer.WriteElementString("UserName", txtUserName.Text);
            writer.WriteElementString("Password", txtPassword .Text);
            writer.WriteElementString("BackupFolder", txtSQLBackup.Text);
            writer.WriteElementString("DataBaseFolder", txtSQLDstPath.Text);

            writer.WriteEndDocument();
            writer.Flush();
            writer.Close();
            this.Close();
        }

        private void btnSqlBackup_Click(object sender, EventArgs e)
        {
            DialogResult result = folderBrowserDialog1.ShowDialog();
            if (result == DialogResult.OK)
            {
                txtSQLBackup.Text = folderBrowserDialog1.SelectedPath.ToString();
            }
        }

        private void btnSqlDestPath_Click(object sender, EventArgs e)
        {
            DialogResult result = folderBrowserDialog1.ShowDialog();
            if (result == DialogResult.OK)
            {
                txtSQLDstPath.Text = folderBrowserDialog1.SelectedPath.ToString();
            }
        }

        private void button1_Click_1(object sender, EventArgs e)
        {
            string connetionString = null;
            SqlConnection cnn;
            connetionString = "Data Source=" + cmbLvServer.Text + ";Initial Catalog=System.Master;User ID=" + txtLiveUser.Text + ";Password=" + txtLivePW.Text + "";
            cnn = new SqlConnection(connetionString);
            try
            {
                cnn.Open();
                MessageBox.Show("Connected successfully to the server");
                llLiveConnection = true;
                cnn.Close();
            }
            catch (Exception ex)
            {
                MessageBox.Show("Failed to connect to the server!," + ex.Message);
                llLiveConnection = false;
            }
        }
    }
}
