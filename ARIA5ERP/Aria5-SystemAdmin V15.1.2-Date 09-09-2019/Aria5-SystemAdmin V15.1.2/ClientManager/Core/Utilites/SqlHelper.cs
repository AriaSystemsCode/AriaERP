using Microsoft.SqlServer.Management.Common;
using Microsoft.SqlServer.Management.Smo;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Data;
using System.Data.Odbc;
using System.Runtime.Serialization;
using Core.Data;
using System.Data.EntityClient;

namespace Core.Utilites
{
    public class SqlHelper
    {
        public static void ExecuteScriptFile(SqlConnection con, string FilepPath, params string[] paramters)
        {
            if (!File.Exists(FilepPath))
                throw new InvalidArgumentException(FilepPath + " Not found!");
            string script = File.ReadAllText(FilepPath);
            if (paramters != null && paramters.Length > 0)
                script = string.Format(script, paramters);
            ExecuteScriptFile(con, script);
        }

        public static void ExecuteScriptFile(SqlConnection con, string StringContent)
        {
            Server server = new Server(new ServerConnection(con));
            int x = server.ConnectionContext.ExecuteNonQuery(StringContent.ToString());
        }

        public static void AttachDB(SqlConnection con, string DataBaseName, string MdfFilePath, string LdfFilePath)
        {
            SqlCommand cmd = new SqlCommand("sp_attach_db", con);
            con.Open();
            cmd.CommandType = CommandType.StoredProcedure;
            cmd.Parameters.Add("@dbname", SqlDbType.NVarChar).Value = DataBaseName;
            cmd.Parameters.Add("@filename1", SqlDbType.NVarChar).Value = MdfFilePath;
            cmd.Parameters.Add("@filename2", SqlDbType.NVarChar).Value = LdfFilePath;
            cmd.ExecuteNonQuery();
        }
        //ATA replace odbc connection with oledb 7/2/2017
        public static OdbcConnection GetFoxConnection(string path)
        {
            string con = "Driver=Microsoft FoxPro VFP Driver (*.dbf);SourceType=DBF;SourceDB={0};Exclusive=No;Collate=Machine;NULL=NO;DELETED=YES;BACKGROUNDFETCH=NO;".Replace("{0}", path);
            //ATA add new function to track the connection 
            System.IO.File.WriteAllText("D:\\TestODBCConnection.TXT", con);
            return new OdbcConnection(con);
        }

        //ATA oledb connection 
        public static System.Data.OleDb.OleDbConnection GetFoxConnection1(string path)
        {
            string con = @"Provider=vfpoledb.1;Data Source={0};Exclusive=false;Nulls=false;Collating Sequence=MACHINE;providerName=System.Data.OleDb".Replace("{0}", path);
            //ATA add new function to track the connection 
            System.IO.File.WriteAllText("D:\\TestODBCConnection.TXT", con);
            return new System.Data.OleDb.OleDbConnection(con);
        }
    }

    public class SQLInfo
    {
        public string ServerName { get; set; }

        public Credential Admin { get; set; }

        public Credential Client { get; set; }

        public string DataBaseName { get; set; }

        public SqlConnection GetConnection()
        {
            string connectionString = string.Format(@"Data Source={0};Initial Catalog={1};User ID={2};Password={3}", ServerName, DataBaseName, Admin.UserName, Admin.Password);
            return new SqlConnection(connectionString);
        }

        public SqlConnection GetMasterConnection()
        {
            string connectionString = string.Format(@"Data Source={0};Initial Catalog={1};User ID={2};Password={3}", ServerName, "Master", Admin.UserName, Admin.Password);
            return new SqlConnection(connectionString);
        }

    }

    [DataContract]
    public class Credential
    {
        public Credential(string UserName, string Password)
        {
            this.UserName = UserName;
            this.Password = Password;
        }

        [DataMember]
        public string UserName { get; set; }

        [DataMember]
        public string Password { get; set; }
    }
}
