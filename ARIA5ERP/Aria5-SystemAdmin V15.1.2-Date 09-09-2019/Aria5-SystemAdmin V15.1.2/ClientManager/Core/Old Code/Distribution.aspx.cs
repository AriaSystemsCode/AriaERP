using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.IO;
using System.Data;
using System.Data.SqlClient;
using System.Data.Odbc;
using System.DirectoryServices;
using System.Security.AccessControl;
using System.Management;
using System.Management.Instrumentation;
using Aria.Environment;


public partial class Distribution : System.Web.UI.Page
{
    public string Report = "";
    private String ADGRPOUP = "group";

    // Grouptype-Definition 
    enum GrpType : uint
    {
        UnivGrp = 0x08,
        DomLocalGrp = 0x04,
        GlobalGrp = 0x02,
        SecurityGrp = 0x80000000
    }
    private static TSUSEREXLib.IADsTSUserEx m_TsUser;
    protected void Page_Load(object sender, EventArgs e)
    {


    }

    AriaEnviromentVariables env = new AriaEnviromentVariables();
    SqlConnection _sqlConn = null;
    /// <summary>
    /// To return SqlConnection from AriaEnviromentVariables and SqlConnection is open.
    /// </summary>
    public SqlConnection SqlConn
    {
        get
        {
            if (_sqlConn == null)
                _sqlConn = new SqlConnection(env.Aria50SystemFilesConnectionString);
            if (_sqlConn.State != ConnectionState.Open)
                _sqlConn.Open();
            return _sqlConn;
        }
    }

    //[Add]Ahmed Maher -Date: 24/03/2010 -AriaEmail
    /// <summary>
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
        sqlConnection = new SqlConnection(@"Data Source=" + ServerName + @";Initial Catalog=Master;User ID=" + Username + @";Password=" + Password);

        SqlCommand cmd = new SqlCommand("sp_attach_db", sqlConnection);
        sqlConnection.Open();
        cmd.CommandType = CommandType.StoredProcedure;
        cmd.Parameters.Add("@dbname", SqlDbType.NVarChar).Value = DataBaseName;
        cmd.Parameters.Add("@filename1", SqlDbType.NVarChar).Value = @DataBaseFullPath;
        cmd.Parameters.Add("@filename2", SqlDbType.NVarChar).Value = @DataBaseFullPath.ToLower().Replace(".mdf", "_log.ldf");
        cmd.ExecuteNonQuery();
        sqlConnection.Close();
    }

    public void CreateXMLSetting(string ClientID, string PathSaved)
    {
        DataSet DS = new DataSet();
        DS.ReadXml(Server.MapPath("") + "\\ClientDB\\Client_Setting.XML");
        DS.Tables[0].Rows[0]["ClientID"] = ClientID;
        DS.WriteXml(PathSaved);
    }
    //[END]

    protected void ExecuteButton_Click(object sender, EventArgs e)
    {
        //Create Folder Under D:\shared ,D:\Shared-T and D:\SQL_DATABASES 
        string SharedPath = @"\\303842-VM4\shared\" + CustCodeTextBox.Text + "SH";
        string SharedTPath = @"\\303842-VM4\Shared-T\" + CustCodeTextBox.Text + "-T";
        string SQL_DATABASESPath = @"\\303842-VM4\SQL_DATABASES\" + CustCodeTextBox.Text;
        string[] Comapnies = CompsNameTextBox.Text.Split('|');
        string[] ComapniesName = TextBox1.Text.Split('|');

        string CreateSQLUser = @"CREATE LOGIN [{0}] WITH 
                                PASSWORD=N'{1}', DEFAULT_DATABASE=[master],DEFAULT_LANGUAGE=[us_english], CHECK_EXPIRATION=OFF, CHECK_POLICY=OFF
                                GO
                                EXEC sys.sp_addsrvrolemember @loginame = N'{0}', @rolename = N'bulkadmin'
                                GO
                                ALTER LOGIN [{0}] enable";
        #region SQL Statement For create Databases
        string CreateSQLDatabase = @"USE [master]
                                    GO
                                    IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = N'{0}')
                                    BEGIN
                                    CREATE DATABASE [{0}] ON  PRIMARY 
                                    ( NAME = N'{0}', FILENAME = N'{1}.mdf' , SIZE = 3072KB , MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB )
                                    LOG ON 
                                    ( NAME = N'{0}_log', FILENAME = N'{1}_log.ldf' , SIZE = 1024KB , MAXSIZE = 2048GB , FILEGROWTH = 10%)
                                    END
                                    GO
                                    EXEC dbo.sp_dbcmptlevel @dbname=N'{0}', @new_cmptlevel=80
                                    GO
                                    IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
                                    begin
                                    EXEC [{0}].[dbo].[sp_fulltext_database] @action = 'enable'
                                    end
                                    GO
                                    ALTER DATABASE [{0}] SET ANSI_NULL_DEFAULT OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET ANSI_NULLS OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET ANSI_PADDING OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET ANSI_WARNINGS OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET ARITHABORT OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET AUTO_CLOSE ON 
                                    GO
                                    ALTER DATABASE [{0}] SET AUTO_CREATE_STATISTICS ON 
                                    GO
                                    ALTER DATABASE [{0}] SET AUTO_SHRINK ON 
                                    GO
                                    ALTER DATABASE [{0}] SET AUTO_UPDATE_STATISTICS ON 
                                    GO
                                    ALTER DATABASE [{0}] SET CURSOR_CLOSE_ON_COMMIT OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET CURSOR_DEFAULT  GLOBAL 
                                    GO
                                    ALTER DATABASE [{0}] SET CONCAT_NULL_YIELDS_NULL OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET NUMERIC_ROUNDABORT OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET QUOTED_IDENTIFIER OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET RECURSIVE_TRIGGERS OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET  DISABLE_BROKER 
                                    GO
                                    ALTER DATABASE [{0}] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET DATE_CORRELATION_OPTIMIZATION OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET TRUSTWORTHY OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET ALLOW_SNAPSHOT_ISOLATION OFF 
                                    GO
                                    ALTER DATABASE [{0}] SET PARAMETERIZATION SIMPLE 
                                    GO
                                    ALTER DATABASE [{0}] SET  READ_WRITE 
                                    GO
                                    ALTER DATABASE [{0}] SET RECOVERY FULL 
                                    GO
                                    ALTER DATABASE [{0}] SET  MULTI_USER 
                                    GO
                                    ALTER DATABASE [{0}] SET PAGE_VERIFY CHECKSUM  
                                    GO
                                    ALTER DATABASE [{0}] SET DB_CHAINING OFF
                                    GO
                                    USE [{0}]
                                    GO
                                    IF NOT EXISTS (SELECT * FROM sys.database_principals WHERE name = N'{2}')
                                    CREATE USER [{2}] FOR LOGIN [{2}] WITH DEFAULT_SCHEMA=[dbo]
                                    GO
                                    IF NOT EXISTS (SELECT * FROM dbo.sysusers WHERE name = N'{2}')
                                    EXEC dbo.sp_grantdbaccess @loginame = N'{2}', @name_in_db = N'{2}'
                                    USE [{0}]
                                    GO
                                    EXEC sp_addrolemember N'db_owner', N'{2}'";

        #endregion
        Report = "";
        if (!Directory.Exists(SharedPath))
        {
            Directory.CreateDirectory(SharedPath);
            Report = Report + @"<span style=""color:Green"">- Create " + SharedPath + " :Done</span><br/>";
            if (!Directory.Exists(SharedTPath))
            {
                Directory.CreateDirectory(SharedTPath);
                Report = Report + @"<span style=""color:Green"">- Create " + SharedTPath + " :Done</span><br/>";
                if (!Directory.Exists(SQL_DATABASESPath))
                {
                    Directory.CreateDirectory(SQL_DATABASESPath);
                    Report = Report + @"<span style=""color:Green"">- Create " + SQL_DATABASESPath + " :Done</span><br/>";

                    // Copy folders 'Aria4XP' and 'Aria27' from path "\\303842-VM4\Distribution" to Customer folder
                    try
                    {
                        CopyFolder(@"\\303842-VM4\Distribution", SharedPath);
                        CopyFolder(@"\\303842-VM4\Distribution", SharedTPath);
                    }
                    catch (Exception Ex)
                    {
                        Report = Report + @"<span style=""color:Red"">- Copy folders 'Aria4XP' and 'Aria27' where <" + Ex.Message + "> :Error</span><br/>";
                    }

                    //Create Sql Server Database
                    CreateSQLUser = string.Format(CreateSQLUser, CustCodeTextBox.Text,
                        CustCodeTextBox.Text.Substring(0, 3).ToLower());
                    SqlConnection Conn = new SqlConnection("Data Source=" + System.Configuration.ConfigurationSettings.AppSettings["SqlServerName"] +
                        ";Initial Catalog=Master;User ID=" + System.Configuration.ConfigurationSettings.AppSettings["SqlUserName"] +
                        ";Password=" + System.Configuration.ConfigurationSettings.AppSettings["SqlPass"]);
                    Conn.Open();
                    string[] splitter = new string[] { "GO\r\n" };
                    string[] commandCreateSQLUser = CreateSQLUser.Split(splitter,
                          StringSplitOptions.RemoveEmptyEntries);
                    int Success1 = 0;
                    foreach (string commandText in commandCreateSQLUser)
                    {
                        SqlCommand CMD = new SqlCommand(commandText, Conn);
                        Success1 = Success1 + CMD.ExecuteNonQuery();
                    }
                    if ((-1 * Success1) == commandCreateSQLUser.Length)
                        Report = Report + @"<span style=""color:Green"">- Create " + CustCodeTextBox.Text + " SQL Database User :Done</span><br/>";
                    else
                        Report = Report + @"<span style=""color:Red"">- Not Create " + CustCodeTextBox.Text + " SQL Database User :Error</span><br/>";

                    //[ADD]Ahmed Maher -Date: 24/03/2010
                    string ServerName = System.Configuration.ConfigurationSettings.AppSettings["SqlServerName"];
                    string DBName = "Aria.Master";
                    string UserName = CustCodeTextBox.Text;
                    string passWord = CustCodeTextBox.Text.Substring(0, 3).ToLower();

                    if (DBNameTextBox.Text.Length > 0)
                        DBName = DBNameTextBox.Text;

                    if (UserNameTextBox.Text.Length > 0)
                        UserName = UserNameTextBox.Text;
                    if (PassTextBox.Text.Length > 0)
                        passWord = PassTextBox.Text;
                    if (ServerTextBox.Text.Length > 0)
                        ServerName = ServerTextBox.Text;

                    //MOH T20100512.0026 Start
                    File.Copy(Server.MapPath("") + "\\ClientDB\\Aria.Master.mdf", SQL_DATABASESPath + "\\" + CustCodeTextBox.Text + ".Master.mdf");
                    File.Copy(Server.MapPath("") + "\\ClientDB\\Aria.Master_log.ldf", SQL_DATABASESPath + "\\" + CustCodeTextBox.Text + ".Master_log.ldf");
                    AttachDataBase(SQL_DATABASESPath + "\\" + CustCodeTextBox.Text + ".Master.mdf", ServerName,
                        CustCodeTextBox.Text + ".Master", UserName, passWord);
                    //AttachDataBase(Server.MapPath("") + "\\ClientDB\\Aria.Master.mdf", ServerName, DBName, UserName, passWord);
                    //MOH T20100512.0026 End
                    //[END]
                    string FoxConntectionString = @"Driver={Microsoft Visual FoxPro Driver};SourceType=DBF;SourceDB=" + SharedPath + "\\ARIA27\\SYSFILES" + ";Exclusive=No;Collate=Machine;NULL=NO;DELETED=YES;BACKGROUNDFETCH=NO;"; ;
                    OdbcConnection FoxConn = new OdbcConnection(FoxConntectionString);
                    FoxConn.Open();
                    int SyccomSuccess = 0;
                    //Delete All rows in Syccomp
                    string SyccompDelete = "DELETE FROM syccomp";
                    OdbcCommand FoxDeleteCMD = new OdbcCommand(SyccompDelete, FoxConn);
                    FoxDeleteCMD.ExecuteNonQuery();
                    #region Create Comapnies Databases
                    for (int i = 0; i < Comapnies.Length + 1; i++)
                    {
                        if (Comapnies.Length == i)
                        {
                            string CreateSQLDatabase1 = string.Format(CreateSQLDatabase, CustCodeTextBox.Text + "_LDB99",
                            SQL_DATABASESPath + "\\" + CustCodeTextBox.Text + "_LDB99",
                            CustCodeTextBox.Text);

                            string[] commandCreateSQLDatabase = CreateSQLDatabase1.Split(splitter,
                              StringSplitOptions.RemoveEmptyEntries);
                            int Success = 0;
                            foreach (string commandText in commandCreateSQLDatabase)
                            {
                                SqlCommand CMD = new SqlCommand(commandText, Conn);
                                Success = Success + CMD.ExecuteNonQuery();
                            }
                            if ((-1 * Success) == commandCreateSQLDatabase.Length)
                                Report = Report + @"<span style=""color:Green"">- Create " + CustCodeTextBox.Text + "_LDB99 SQL Database :Done</span><br/>";
                            else
                                Report = Report + @"<span style=""color:Red"">- Not Create " + CustCodeTextBox.Text + "_LDB99 Database :Error</span><br/>";

                            //Update Syccomp
                            string InsertStatement = "insert INTO syccomp (ccomp_id,Ccom_name,Ccom_ddir,Ccondriver,cconserver,ccondbname,cconuserid,cconpaswrd,Lrunfroma4) VALUES ('" +
                                "99" + "','Demo company','X:\\Aria27\\DBFs\\99\\','SQL','247968-TERMINAL\\AriaSql','" +
                                CustCodeTextBox.Text + "_LDB99','" +
                                CustCodeTextBox.Text + "','" + CustCodeTextBox.Text.Substring(0, 3).ToLower() + "',1)";
                            OdbcCommand FoxCMD = new OdbcCommand(InsertStatement, FoxConn);
                            SyccomSuccess = SyccomSuccess + FoxCMD.ExecuteNonQuery();
                        }
                        else
                        {
                            string CreateSQLDatabase1 = string.Format(CreateSQLDatabase, CustCodeTextBox.Text + "_LDB" + Comapnies[i],
                                SQL_DATABASESPath + "\\" + CustCodeTextBox.Text + "_LDB" + Comapnies[i],
                                CustCodeTextBox.Text);

                            string[] commandCreateSQLDatabase = CreateSQLDatabase1.Split(splitter,
                              StringSplitOptions.RemoveEmptyEntries);
                            int Success = 0;
                            foreach (string commandText in commandCreateSQLDatabase)
                            {
                                SqlCommand CMD = new SqlCommand(commandText, Conn);
                                Success = Success + CMD.ExecuteNonQuery();
                            }
                            if ((-1 * Success) == commandCreateSQLDatabase.Length)
                                Report = Report + @"<span style=""color:Green"">- Create " + CustCodeTextBox.Text + "_LDB" + Comapnies[i] + " SQL Database :Done</span><br/>";
                            else
                                Report = Report + @"<span style=""color:Red"">- Not Create " + CustCodeTextBox.Text + "_LDB" + Comapnies[i] + " Database :Error</span><br/>";

                            //Update Syccomp
                            string InsertStatement = "insert INTO syccomp (ccomp_id,Ccom_name,Ccom_ddir,Ccondriver,cconserver,ccondbname,cconuserid,cconpaswrd,Lrunfroma4) VALUES ('" +
                                Comapnies[i] + "','" + ComapniesName[i] + "','X:\\Aria27\\DBFs\\" + Comapnies[i] + "\\','SQL','247968-TERMINAL\\AriaSql','" +
                                CustCodeTextBox.Text + "_LDB" + Comapnies[i] + "','" +
                                CustCodeTextBox.Text + "','" + CustCodeTextBox.Text.Substring(0, 3).ToLower() + "',1)";
                            OdbcCommand FoxCMD = new OdbcCommand(InsertStatement, FoxConn);
                            SyccomSuccess = SyccomSuccess + FoxCMD.ExecuteNonQuery();
                        }
                    }
                    //Create Database for test
                    string FoxTestConntectionString = @"Driver={Microsoft Visual FoxPro Driver};SourceType=DBF;SourceDB=" + SharedTPath + "\\ARIA27\\SYSFILES" + ";Exclusive=No;Collate=Machine;NULL=NO;DELETED=YES;BACKGROUNDFETCH=NO;"; ;
                    OdbcConnection FoxConnTest = new OdbcConnection(FoxTestConntectionString);
                    FoxConnTest.Open();
                    OdbcCommand TestCMDSyccomp = new OdbcCommand(SyccompDelete, FoxConnTest);
                    TestCMDSyccomp.ExecuteNonQuery();

                    string CreateSQLDatabaseTest = string.Format(CreateSQLDatabase, CustCodeTextBox.Text + "_TDB" + Comapnies[0],
                            SQL_DATABASESPath + "\\" + CustCodeTextBox.Text + "_TDB" + Comapnies[0],
                            CustCodeTextBox.Text);

                    string[] commandCreateSQLDatabaseTest = CreateSQLDatabaseTest.Split(splitter,
                      StringSplitOptions.RemoveEmptyEntries);
                    int Suc = 0;
                    foreach (string commandText in commandCreateSQLDatabaseTest)
                    {
                        SqlCommand CMD = new SqlCommand(commandText, Conn);
                        Suc = Suc + CMD.ExecuteNonQuery();
                    }
                    if ((-1 * Suc) == commandCreateSQLDatabaseTest.Length)
                        Report = Report + @"<span style=""color:Green"">- Create " + CustCodeTextBox.Text + "_TDB" + Comapnies[0] + " SQL Database :Done</span><br/>";
                    else
                        Report = Report + @"<span style=""color:Red"">- Not Create " + CustCodeTextBox.Text + "_TDB" + Comapnies[0] + " Database :Error</span><br/>";

                    //Update Syccomp
                    string InsertStatementTest = "insert INTO syccomp (ccomp_id,Ccom_name,Ccom_ddir,Ccondriver,cconserver,ccondbname,cconuserid,cconpaswrd,Lrunfroma4) VALUES ('" +
                        Comapnies[0] + "','" + ComapniesName[0] + " Test','X:\\Aria27\\DBFs\\" + Comapnies[0] + "\\','SQL','247968-TERMINAL\\AriaSql','" +
                        CustCodeTextBox.Text + "_TDB" + Comapnies[0] + "','" +
                        CustCodeTextBox.Text + "','" + CustCodeTextBox.Text.Substring(0, 3).ToLower() + "',1)";
                    OdbcCommand FoxCMDTest = new OdbcCommand(InsertStatementTest, FoxConnTest);
                    SyccomSuccess = SyccomSuccess + FoxCMDTest.ExecuteNonQuery();

                    //if ((SyccomSuccess) == Comapnies.Length + 1)
                    //    Report = Report + @"<span style=""color:Green"">- Insert into Syccomp :Done</span><br/>";
                    //else
                    //    Report = Report + @"<span style=""color:Red"">- Fail Insert into Syccomp :Error</span><br/>";
                    #endregion

                    #region Add Users and group
                    //1- Add Group
                    using (DirectoryEntry myDE = new DirectoryEntry("LDAP://CN=Users,DC=aria,DC=local",
                        System.Configuration.ConfigurationSettings.AppSettings["ADUserName"],
                        System.Configuration.ConfigurationSettings.AppSettings["ADPassword"]))
                    //using (DirectoryEntry myDE = new DirectoryEntry("LDAP://aria","ahmed.r","aria_123"))
                    {
                        DirectoryEntries myEntries = myDE.Children;
                        using (DirectoryEntry group = myDE.Children.Add("CN=" + CustNameTextBox.Text, "group"))
                        {
                            GrpType gt = GrpType.GlobalGrp | GrpType.SecurityGrp;
                            int typeNum = (int)gt;
                            group.Properties["sAMAccountName"].Add(CustNameTextBox.Text);
                            group.Properties["description"].Add(CustNameTextBox.Text);
                            group.Properties["groupType"].Add(typeNum);
                            group.CommitChanges();
                        }
                        for (int i = 1; i <= Convert.ToInt32(NumDomainUsersTextBox.Text); i++)
                        {
                            using (DirectoryEntry group = myDE.Children.Add("CN=" + CustCodeTextBox.Text + "_U" + i.ToString(), "user"))
                            {
                                group.Properties["sAMAccountName"].Add(CustCodeTextBox.Text + "_U" + i.ToString());
                                group.Properties["homedrive"].Add("X:");
                                group.Properties["homedirectory"].Add(@"\\247968-TERMINAL\" + CustCodeTextBox.Text + "SH");
                                group.Properties["displayName"].Add(CustCodeTextBox.Text + "_U" + i.ToString());
                                group.Properties["description"].Add(CustCodeTextBox.Text.Substring(0, 3).ToLower()
                                    + "_" + CustPhoneTextBox.Text);
                                ActiveDs.IADsUser iADsUser = (ActiveDs.IADsUser)group.NativeObject;
                                m_TsUser = (TSUSEREXLib.IADsTSUserEx)iADsUser;
                                m_TsUser.TerminalServicesInitialProgram = @"D:\shared\Aria27\AAS_WIN.EXE"; //For Example
                                m_TsUser.TerminalServicesWorkDirectory = @"D:\shared\Aria27\";
                                group.CommitChanges();

                                group.Invoke("SetPassword", new object[] { CustCodeTextBox.Text.Substring(0, 3).ToLower() + "_" + CustPhoneTextBox.Text });
                                group.CommitChanges();

                                int val = (int)group.Properties["userAccountControl"].Value;
                                group.Properties["userAccountControl"].Value = val & ~0x2;
                                group.CommitChanges();

                                DirectorySearcher deSearch = new DirectorySearcher();
                                deSearch.SearchRoot = myDE;
                                deSearch.Filter = "(&(objectClass=group) (cn=" + CustNameTextBox.Text + "))";
                                DirectoryEntry dirEntry = deSearch.FindOne().GetDirectoryEntry();
                                dirEntry.Invoke("Add", new object[] { group.Path.ToString() });
                                dirEntry.CommitChanges();
                                deSearch.SearchRoot = new DirectoryEntry("LDAP://CN=Builtin,DC=aria,DC=local",
                                    System.Configuration.ConfigurationSettings.AppSettings["ADUserName"],
                                    System.Configuration.ConfigurationSettings.AppSettings["ADPassword"]);
                                deSearch.Filter = "(&(objectClass=group) (cn=Users))";
                                DirectoryEntry dirEntry2 = deSearch.FindOne().GetDirectoryEntry();
                                dirEntry2.Invoke("Add", new object[] { group.Path.ToString() });
                                dirEntry2.CommitChanges();
                                deSearch.Filter = "(&(objectClass=group) (cn=Remote Desktop Users))";
                                DirectoryEntry dirEntry1 = deSearch.FindOne().GetDirectoryEntry();
                                dirEntry1.Invoke("Add", new object[] { group.Path.ToString() });
                                dirEntry1.CommitChanges();

                                int ADS_UF_NORMAL_ACCOUNT = 512;
                                int combinedFlag = ADS_UF_NORMAL_ACCOUNT;
                                //1. User cannot change password
                                //int ADS_UF_PASSWD_CANT_CHANGE = 0x000000040;
                                //2. Password Never Expires
                                int ADS_UF_DONT_EXPIRE_PASSWD = 0x00010000;
                                combinedFlag = combinedFlag | ADS_UF_DONT_EXPIRE_PASSWD;
                                group.Invoke("Put", new Object[] { "userAccountControl", combinedFlag });
                                group.CommitChanges();
                            }
                        }
                        #region Add Test User
                        using (DirectoryEntry group = myDE.Children.Add("CN=" + CustCodeTextBox.Text + "_T1", "user"))
                        {
                            group.Properties["sAMAccountName"].Add(CustCodeTextBox.Text + "_T1");
                            group.Properties["homedrive"].Add("X:");
                            group.Properties["homedirectory"].Add(@"\\247968-TERMINAL\" + CustCodeTextBox.Text + "-T");
                            group.Properties["displayName"].Add(CustCodeTextBox.Text + "_T1");
                            group.Properties["description"].Add(CustCodeTextBox.Text.Substring(0, 3).ToLower()
                                + "_" + CustPhoneTextBox.Text);
                            ActiveDs.IADsUser iADsUser = (ActiveDs.IADsUser)group.NativeObject;
                            m_TsUser = (TSUSEREXLib.IADsTSUserEx)iADsUser;
                            m_TsUser.TerminalServicesInitialProgram = @"D:\shared\Aria27\AAS_WIN.EXE"; //For Example
                            m_TsUser.TerminalServicesWorkDirectory = @"D:\shared\Aria27\";
                            group.CommitChanges();

                            group.Invoke("SetPassword", new object[] { CustCodeTextBox.Text.Substring(0, 3).ToLower() + "_" + CustPhoneTextBox.Text });
                            group.CommitChanges();

                            int val = (int)group.Properties["userAccountControl"].Value;
                            group.Properties["userAccountControl"].Value = val & ~0x2;
                            group.CommitChanges();

                            DirectorySearcher deSearch = new DirectorySearcher();
                            deSearch.SearchRoot = myDE;
                            deSearch.Filter = "(&(objectClass=group) (cn=" + CustNameTextBox.Text + "))";
                            DirectoryEntry dirEntry = deSearch.FindOne().GetDirectoryEntry();
                            dirEntry.Invoke("Add", new object[] { group.Path.ToString() });
                            dirEntry.CommitChanges();
                            deSearch.SearchRoot = new DirectoryEntry("LDAP://CN=Builtin,DC=aria,DC=local",
                                System.Configuration.ConfigurationSettings.AppSettings["ADUserName"],
                                System.Configuration.ConfigurationSettings.AppSettings["ADPassword"]);
                            deSearch.Filter = "(&(objectClass=group) (cn=Users))";
                            DirectoryEntry dirEntry2 = deSearch.FindOne().GetDirectoryEntry();
                            dirEntry2.Invoke("Add", new object[] { group.Path.ToString() });
                            dirEntry2.CommitChanges();
                            deSearch.Filter = "(&(objectClass=group) (cn=Remote Desktop Users))";
                            DirectoryEntry dirEntry1 = deSearch.FindOne().GetDirectoryEntry();
                            dirEntry1.Invoke("Add", new object[] { group.Path.ToString() });
                            dirEntry1.CommitChanges();

                            int ADS_UF_NORMAL_ACCOUNT = 512;
                            int combinedFlag = ADS_UF_NORMAL_ACCOUNT;
                            //1. User cannot change password
                            //int ADS_UF_PASSWD_CANT_CHANGE = 0x000000040;
                            //2. Password Never Expires
                            int ADS_UF_DONT_EXPIRE_PASSWD = 0x00010000;
                            combinedFlag = combinedFlag | ADS_UF_DONT_EXPIRE_PASSWD;
                            group.Invoke("Put", new Object[] { "userAccountControl", combinedFlag });
                            group.CommitChanges();
                        }
                        #endregion
                    }
                    #endregion

                    #region Shared and add security
                    //For Shared
                    DirectoryInfo myDirectoryInfo = new DirectoryInfo(SharedPath);
                    DirectorySecurity myDirectorySecurity = myDirectoryInfo.GetAccessControl();
                    myDirectorySecurity.AddAccessRule(new FileSystemAccessRule("aria\\" + CustNameTextBox.Text
                        , FileSystemRights.FullControl, AccessControlType.Allow));
                    myDirectoryInfo.SetAccessControl(myDirectorySecurity);

                    ManagementClass managementClass = new ManagementClass("Win32_Share");
                    ManagementBaseObject inParams = managementClass.GetMethodParameters("Create");
                    ManagementBaseObject outParams;
                    inParams["Description"] = CustNameTextBox.Text;
                    inParams["Name"] = CustCodeTextBox.Text + "SH";
                    inParams["Path"] = SharedPath;
                    inParams["Type"] = 0x0; // Disk Drive
                    outParams = managementClass.InvokeMethod("Create", inParams, null);
                    if ((uint)(outParams.Properties["ReturnValue"].Value) != 0)
                    {
                        Report = Report + @"<span style=""color:Red"">- Shared not complete success for(Shared Folder) :Exit</span><br/>";
                    }

                    //For Shared-T
                    DirectoryInfo myDirectoryInfoT = new DirectoryInfo(SharedTPath);
                    DirectorySecurity myDirectorySecurityT = myDirectoryInfoT.GetAccessControl();
                    myDirectorySecurityT.AddAccessRule(new FileSystemAccessRule("aria\\" + CustNameTextBox.Text
                        , FileSystemRights.FullControl, AccessControlType.Allow));
                    myDirectoryInfo.SetAccessControl(myDirectorySecurityT);

                    ManagementClass managementClassT = new ManagementClass("Win32_Share");
                    ManagementBaseObject inParamsT = managementClassT.GetMethodParameters("Create");
                    ManagementBaseObject outParamsT;
                    inParamsT["Description"] = CustNameTextBox.Text;
                    inParamsT["Name"] = CustCodeTextBox.Text + "-T";
                    inParamsT["Path"] = SharedTPath;
                    inParamsT["Type"] = 0x0; // Disk Drive
                    outParamsT = managementClassT.InvokeMethod("Create", inParamsT, null);
                    if ((uint)(outParamsT.Properties["ReturnValue"].Value) != 0)
                    {
                        Report = Report + @"<span style=""color:Red"">- Shared not complete success for(Shared-T Folder) :Exit</span><br/>";
                    }
                    #endregion

                    int SuccessInsert = 0;
                    Conn.Close();
                    Conn.ConnectionString = "Data Source=" + System.Configuration.ConfigurationSettings.AppSettings["SqlServerName"] +
                        ";Initial Catalog=" + System.Configuration.ConfigurationSettings.AppSettings["DatabaseName"] +
                        ";User ID=" + System.Configuration.ConfigurationSettings.AppSettings["SqlUserName"] +
                        ";Password=" + System.Configuration.ConfigurationSettings.AppSettings["SqlPass"];
                    Conn.Open();
                    if (!(Conn.State == ConnectionState.Open))
                        Conn.Open();

                    //[ADD]Ahmed Maher -Date: 24/03/2010
                    //string ClientInsert = "INSERT INTO CLIENTS([CCLIENTID],[CCLIENTNAME],[CDATAPATH],[LLOCKSYS]) VALUES('" +
                    //    CustCodeTextBox.Text + "','" + CustNameTextBox.Text + "','" +
                    //    SharedPath + "',0)";

                    //xx=SharedPath.replace("SQLDictionary","")
                    //Mostafa T20100512.0026 Start
                    //CreateXMLSetting(CustCodeTextBox.Text, SharedTPath + "\\Client_Setting.XML");
                    //CreateXMLSetting(CustCodeTextBox.Text, SharedPath + "\\Client_Setting.XML");

                    CreateXMLSetting(CustCodeTextBox.Text, SharedTPath + "\\Aria4xp" + "\\Client_Setting.XML");
                    CreateXMLSetting(CustCodeTextBox.Text, SharedPath + "\\Aria4xp" + "\\Client_Setting.XML");
                    //Mostafa T20100512.0026 End
                    //string ClientInsert = "INSERT INTO CLIENTS([CCLIENTID],[CCLIENTNAME],[CDATAPATH],[LLOCKSYS],[CCONSERVER],[CCONDBNAME],[CCONUSERID],[CCONPASWRD]) VALUES('" +
                    //                       CustCodeTextBox.Text.ToUpper() + "','" + CustNameTextBox.Text + "','" +
                    //                       SharedPath.ToUpper() + "',0,'" + ServerName + "','" + DBName + "','" + UserName + "','" + passWord + "')";


                    string ClientInsert = "INSERT INTO CLIENTS([CCLIENTID],[CCLIENTNAME],[CDATAPATH],[LLOCKSYS],[CCONSERVER],[CCONDBNAME],[CCONUSERID],[CCONPASWRD],[Aria27sys],[Aria40sys]) VALUES('" +
                        CustCodeTextBox.Text.ToUpper() + "','" + CustNameTextBox.Text + "','" +
                        SharedPath.ToUpper() + "\\" + "',0,'" + ServerName + "','" + DBName + "','" + UserName + "','" + passWord + "','" + SharedPath.ToUpper() + @"\ARIA27\SYSFILES\" + "','" + SharedPath.ToUpper() + @"\ARIA4XP\SQLDICTIONARY\')";
                    //[END]
                    SqlCommand ClientCMD = new SqlCommand(ClientInsert, Conn);
                    SuccessInsert = SuccessInsert + ClientCMD.ExecuteNonQuery();
                    for (int i = 1; i <= Convert.ToInt32(NumDomainUsersTextBox.Text); i++)
                    {
                        string UsersInsert = "INSERT INTO AriaUser([UserId],[MenuId],[ClientID],[Description]) VALUES('" +
                            CustCodeTextBox.Text + "_U" + i.ToString() + "',1,'" +
                            CustCodeTextBox.Text + "','" +
                            CustCodeTextBox.Text.Substring(0, 3).ToLower() + CustPhoneTextBox.Text + "')";
                        ClientCMD = new SqlCommand(UsersInsert, Conn);
                        SuccessInsert = SuccessInsert + ClientCMD.ExecuteNonQuery();
                    }

                    int TotalUpdate = 1 + Convert.ToInt32(NumDomainUsersTextBox.Text);
                    #region Update table in System.Master

                    #endregion

                    #region Delete unwanted files and folder
                    DirectoryInfo DireInfo = new DirectoryInfo(SharedPath);

                    string[] SubDirShared = Directory.GetDirectories(SharedPath);
                    foreach (string folder in SubDirShared)
                    {
                        DirectoryInfo dir = new DirectoryInfo(folder);
                        if (dir.Name != "Aria4XP" && dir.Name != "ARIA27")
                            Directory.Delete(folder, true);
                    }
                    string[] SubDirSharedT = Directory.GetDirectories(SharedTPath);
                    foreach (string folder in SubDirSharedT)
                    {
                        DirectoryInfo dir = new DirectoryInfo(folder);
                        if (dir.Name != "Aria4XP" && dir.Name != "ARIA27")
                            Directory.Delete(folder, true);
                    }
                    string[] SubFileShared = Directory.GetFiles(SharedPath);
                    foreach (string file in SubFileShared)
                    {
                        File.Delete(file);
                    }
                    string[] SubFileSharedT = Directory.GetFiles(SharedTPath);
                    foreach (string file in SubFileSharedT)
                    {
                        File.Delete(file);
                    }
                    //string[] CompanyShared = Directory.GetDirectories(SharedPath + "\\ARIA27\\DBFs");
                    //foreach (string folder in SubFileSharedT)
                    //{
                    //    if (folder.Replace(SharedPath + "\\ARIA27\\DBFs\\", "") != "99")
                    //        File.Delete(folder);
                    //}
                    #endregion

                    #region Copy SYCINST
                    File.Copy(@"\\303842-VM4\Distribution\TestSYSFILES\SYCINST.DBF",
                        SharedTPath + @"\Aria27\SYSFILES\SYCINST.DBF", true);
                    #endregion
                    //Go To Next view
                    string SelectStatement = "Select * from sydappl";
                    OdbcCommand FoxCMDSyd = new OdbcCommand(SelectStatement, FoxConn);
                    DataTable DT = new DataTable();
                    DT.Load(FoxCMDSyd.ExecuteReader());
                    SydApplCheckBoxList.DataSource = DT;
                    SydApplCheckBoxList.DataTextField = "capp_name";
                    SydApplCheckBoxList.DataValueField = "capp_id";
                    SydApplCheckBoxList.DataBind();
                    DisMultiView.ActiveViewIndex = 1;

                    Conn.Close();
                    FoxConn.Close();
                    FoxConnTest.Close();
                }
                else
                    Report = Report + @"<span style=""color:Red"">- Create " + SQL_DATABASESPath + " :Exit</span><br/>";
            }
            else
                Report = Report + @"<span style=""color:Red"">- Create " + SharedTPath + " :Exit</span><br/>";
        }
        else
            Report = Report + @"<span style=""color:Red"">- Create " + SharedPath + " :Exit</span><br/>";

        ErrorLabel.Text = Report;
        Report = "";
    }

    static public void CopyFolder(string sourceFolder, string destFolder)
    {
        int x = 0;
        if (!Directory.Exists(destFolder))
            Directory.CreateDirectory(destFolder);
        string[] files = Directory.GetFiles(sourceFolder);
        foreach (string file in files)
        {
            string name = Path.GetFileName(file);
            string dest = Path.Combine(destFolder, name);
            File.Copy(file, dest);
        }
        string[] folders = Directory.GetDirectories(sourceFolder);
        foreach (string folder in folders)
        {
            string name = Path.GetFileName(folder);
            string dest = Path.Combine(destFolder, name);

            if (folder.Contains("\\dbfs"))
            {
                if (folder.Substring(folder.Length - 4) == "dbfs")
                {
                    CopyFolder(folder, dest);
                    goto xx;
                }
                if (folder.Substring(folder.Length - 2) == "99")
                {
                    CopyFolder(folder, dest);
                    goto xx;
                }
                else
                    goto xx;
            }
            CopyFolder(folder, dest);
        xx:
            x = 0;
        }
    }
    protected void SydappButton_Click(object sender, EventArgs e)
    {
        ErrorLabel.Text = "";
        try
        {
            string FoxConntectionString = @"Driver={Microsoft Visual FoxPro Driver};SourceType=DBF;SourceDB=" + @"D:\shared\" + CustCodeTextBox.Text + "SH" + "\\ARIA27\\SYSFILES" + ";Exclusive=No;Collate=Machine;NULL=NO;DELETED=YES;BACKGROUNDFETCH=NO;";
            string FoxConntectionStringT = @"Driver={Microsoft Visual FoxPro Driver};SourceType=DBF;SourceDB=" + @"D:\Shared-T\" + CustCodeTextBox.Text + "-T" + "\\ARIA27\\SYSFILES" + ";Exclusive=No;Collate=Machine;NULL=NO;DELETED=YES;BACKGROUNDFETCH=NO;"; ;
            OdbcConnection FoxConn = new OdbcConnection(FoxConntectionString);
            OdbcConnection FoxConnT = new OdbcConnection(FoxConntectionStringT);
            FoxConnT.Open();
            if (!(FoxConn.State == ConnectionState.Open))
                FoxConn.Open();
            if (!(FoxConnT.State == ConnectionState.Open))
                FoxConnT.Open();
            if (File.Exists(ActKeyTextBox.Text))
            {
                if (File.Exists(@"D:\shared\" + CustCodeTextBox.Text + "SH"
                    + "\\ARIA27\\SYSFILES\\ACT_KEY.BIN"))
                    File.Delete(@"D:\shared\" + CustCodeTextBox.Text + "SH"
                    + "\\ARIA27\\SYSFILES\\ACT_KEY.BIN");
                File.Copy(ActKeyTextBox.Text, @"D:\shared\" + CustCodeTextBox.Text + "SH"
                    + "\\ARIA27\\SYSFILES\\ACT_KEY.BIN");
                Report = Report + @"<span style=""color:Green"">- ACT_KEY Upload Success into shared :Error</span><br/>";
                if (File.Exists(@"D:\Shared-T\" + CustCodeTextBox.Text + "-T" + "\\ARIA27\\SYSFILES\\ACT_KEY.BIN"))
                    File.Delete(@"D:\Shared-T\" + CustCodeTextBox.Text + "-T" + "\\ARIA27\\SYSFILES\\ACT_KEY.BIN");
                File.Copy(ActKeyTextBox.Text, @"D:\Shared-T\" + CustCodeTextBox.Text + "-T" + "\\ARIA27\\SYSFILES\\ACT_KEY.BIN");
                Report = Report + @"<span style=""color:Green"">- ACT_KEY Upload Success into shared-t :Error</span><br/>";

                int numOfChecked = 0;
                int numOfExecuteSuccess = 0;
                int numOfExecuteSuccessT = 0;
                for (int i = 0; i < SydApplCheckBoxList.Items.Count; i++)
                {
                    if (!SydApplCheckBoxList.Items[i].Selected)
                    {
                        numOfChecked = numOfChecked + 1;
                        string DeleteStatement = "DELETE FROM sydappl where capp_id='" + SydApplCheckBoxList.Items[i].Value + "'";
                        OdbcCommand FoxCMD = new OdbcCommand(DeleteStatement, FoxConn);
                        OdbcCommand FoxCMDT = new OdbcCommand(DeleteStatement, FoxConnT);
                        numOfExecuteSuccess = numOfExecuteSuccess + FoxCMD.ExecuteNonQuery();
                        numOfExecuteSuccessT = numOfExecuteSuccessT + FoxCMDT.ExecuteNonQuery();
                    }
                }
                if ((numOfChecked) == numOfExecuteSuccess)
                    Report = Report + @"<span style=""color:Green"">- Sydappl Update Success (Shared) <Num Of Module=" + numOfChecked + ">:Done</span><br/>";
                else
                    Report = Report + @"<span style=""color:Red"">- Sydappl not Update Success (Shared) <Num Of Module=" + numOfExecuteSuccess + " where must be equal=" + numOfChecked + ">:Error</span><br/>";
                if ((numOfChecked) == numOfExecuteSuccessT)
                    Report = Report + @"<span style=""color:Green"">- Sydappl Update Success (Shared-T) <Num Of Module=" + numOfChecked + ">:Done</span><br/>";
                else
                    Report = Report + @"<span style=""color:Red"">- Sydappl not Update Success (Shared-T) <Num Of Module=" + numOfExecuteSuccess + " where must be equal=" + numOfChecked + ">:Error</span><br/>";

                ErrorLabel.Text = Report;
                SqlConnection Conn = new SqlConnection("Data Source=" + System.Configuration.ConfigurationSettings.AppSettings["SqlServerName"] +
                    ";Initial Catalog=" + System.Configuration.ConfigurationSettings.AppSettings["DatabaseName"] +
                    ";User ID=" + System.Configuration.ConfigurationSettings.AppSettings["SqlUserName"] +
                    ";Password=" + System.Configuration.ConfigurationSettings.AppSettings["SqlPass"]);

                Conn.Open();
                string SelectStatement = "Select * from AriaRole";
                SqlCommand RoleCMD = new SqlCommand(SelectStatement, Conn);
                DataTable DT = new DataTable();
                DT.Load(RoleCMD.ExecuteReader());
                AppCheckBoxList.DataSource = DT;
                AppCheckBoxList.DataTextField = "RoleName";
                AppCheckBoxList.DataValueField = "RoleId";
                AppCheckBoxList.DataBind();
                DisMultiView.ActiveViewIndex = 2;
                FoxConn.Close();
                FoxConnT.Close();
                Conn.Close();
            }
            else
                Report = Report + @"<span style=""color:Red"">- File not " + ActKeyTextBox.Text + " Exit :Error</span><br/>";
            //ImageUpload.PostedFile.SaveAs(@"D:\shared\" + CustCodeTextBox.Text + "SH"
            //    + "\\ARIA27\\SYSFILES");

        }
        catch (Exception Ex)
        {
            Report = Report + @"<span style=""color:Red"">- " + Ex.Message + " :Error</span><br/>";
        }
        ErrorLabel.Text = Report;
        Report = "";
    }
    protected void ApplButton_Click(object sender, EventArgs e)
    {
        SqlConnection Conn = new SqlConnection("Data Source=" + System.Configuration.ConfigurationSettings.AppSettings["SqlServerName"] +
                    ";Initial Catalog=" + System.Configuration.ConfigurationSettings.AppSettings["DatabaseName"] +
                    ";User ID=" + System.Configuration.ConfigurationSettings.AppSettings["SqlUserName"] +
                    ";Password=" + System.Configuration.ConfigurationSettings.AppSettings["SqlPass"]);
        Conn.Open();
        //Insert UserRole
        for (int i = 0; i < AppCheckBoxList.Items.Count; i++)
        {
            if (AppCheckBoxList.Items[i].Selected)
            {
                for (int j = 1; j <= Convert.ToInt32(NumDomainUsersTextBox.Text); j++)
                {
                    string InsertUserRole = "INSERT INTO [AriaUserRole]([UserId],[RoleId]) VALUES('" +
                        CustCodeTextBox.Text + "_U" + j.ToString() + "','" +
                        AppCheckBoxList.Items[i].Value + "')";
                    SqlCommand UserRoleCMD = new SqlCommand(InsertUserRole, Conn);
                    UserRoleCMD.ExecuteNonQuery();
                }
                string InsertClientProduct = "";
                if (AppCheckBoxList.Items[i].Value.Trim() == "ARIA27    ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                        CustCodeTextBox.Text + "','" +
                        AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.180</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>ARIA27</remoteapplicationname>\r\n    <remoteapplicationprogram>ARIA27</remoteapplicationprogram>\r\n    <remoteapplicationmode>0</remoteapplicationmode>\r\n <alternateshell>ARIA27</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                if (AppCheckBoxList.Items[i].Value.Trim() == "ARIA4XP   ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.182</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>ARIA4XP</remoteapplicationname>\r\n    <remoteapplicationprogram>ARIA4XP</remoteapplicationprogram>\r\n   <remoteapplicationmode>1</remoteapplicationmode>\r\n <alternateshell>Aria4XP</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                if (AppCheckBoxList.Items[i].Value.Trim() == "AriaEDI   ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.182</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>Aria_EDI</remoteapplicationname>\r\n    <remoteapplicationprogram>Aria_EDI</remoteapplicationprogram>\r\n   <remoteapplicationmode>1</remoteapplicationmode>\r\n  <alternateshell>Aria_EDI</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                
                if (AppCheckBoxList.Items[i].Value.Trim() == "COREFTP   ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.178</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>Aria_EDI_VAN</remoteapplicationname>\r\n    <remoteapplicationprogram>Aria_EDI_VAN</remoteapplicationprogram>\r\n    <remoteapplicationmode>1</remoteapplicationmode>\r\n  <alternateshell>Aria_EDI_VAN</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                
                if (AppCheckBoxList.Items[i].Value.Trim() == "UPS       ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.178</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline></remoteapplicationcmdline>\r\n    <remoteapplicationname></remoteapplicationname>\r\n    <remoteapplicationprogram></remoteapplicationprogram>\r\n    <remoteapplicationmode>0</remoteapplicationmode>\r\n  <alternateshell></alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                
                if (AppCheckBoxList.Items[i].Value.Trim() == "CRM       ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.180</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>../CRMBRISCO/LOGIN.ASP</remoteapplicationname>\r\n    <remoteapplicationprogram>ARIA27</remoteapplicationprogram>\r\n    <remoteapplicationmode>0</remoteapplicationmode>\r\n <alternateshell>ARIA27</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                
                if (AppCheckBoxList.Items[i].Value.Trim() == "AriaRW    ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.178</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>Aria Report Writer</remoteapplicationname>\r\n    <remoteapplicationprogram>Aria Report Writer</remoteapplicationprogram>\r\n    <remoteapplicationmode>1</remoteapplicationmode>\r\n  <alternateshell>Aria Report Writer</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                
                if (AppCheckBoxList.Items[i].Value.Trim() == "AriaVB    ".Trim())
                    InsertClientProduct = "INSERT INTO [AriaClientProduct] ([ClientId],[ProductId],[ProductSettings]) VALUES('" +
                            CustCodeTextBox.Text + "','" +
                            AppCheckBoxList.Items[i].Value + "','<ProductSetting>\r\n  <Product>\r\n    <IPAddress>174.143.166.180</IPAddress>\r\n    <Username>AIC10</Username>\r\n    <Password></Password>\r\n    <remoteapplicationcmdline>X:\\ARIA27\\SYSFILES\\</remoteapplicationcmdline>\r\n    <remoteapplicationname>AriaVB</remoteapplicationname>\r\n    <remoteapplicationprogram>AriaVB</remoteapplicationprogram>\r\n    <remoteapplicationmode>0</remoteapplicationmode>\r\n <alternateshell>AriaVB</alternateshell>\r\n  </Product>\r\n</ProductSetting>')";
                SqlCommand ClientProduct = new SqlCommand(InsertClientProduct, Conn);

                ClientProduct.ExecuteNonQuery();
            }
        }
        Conn.Close();
        ErrorLabel.Text = "";
        DisMultiView.ActiveViewIndex = 3;
    }
}