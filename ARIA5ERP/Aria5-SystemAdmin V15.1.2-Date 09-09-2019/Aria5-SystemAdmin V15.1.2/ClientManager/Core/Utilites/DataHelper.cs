using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Core.Utilites
{
    public class DataHelper
    {

        public static List<KeyValuePair<string, string>> GetAllSystemMasterRoles(SQLInfo SystemMasterSqlInfo)
        {
            Data.SystemMaster db = new Data.SystemMaster(SystemMasterSqlInfo);
            var roles = db.AriaRoles.OrderBy(x => x.RoleName).ToList().Select(x => new KeyValuePair<string, string>(x.RoleId.Trim(), x.RoleName.Trim())).ToList();
            return roles;
        }

        public static List<KeyValuePair<string, string>> GetSydApp(string SharedPath)
        {
            List<KeyValuePair<string, string>> result = new List<KeyValuePair<string, string>>();
            string file = string.Format("{0}Aria4XP\\SYSFILES", SharedPath.EndsWithBackSlash());
            if (!File.Exists(file + "\\sydappl.dbf"))
               throw new FileNotFoundException("File not found:" + file + "\\sydappl.dbf");
            using (var cmd = SqlHelper.GetFoxConnection(file).CreateCommand())
            {
                cmd.CommandText = "Select capp_name,capp_id from sydappl order by capp_name";
                System.Data.Odbc.OdbcDataAdapter da = new System.Data.Odbc.OdbcDataAdapter(cmd);
                DataTable table = new DataTable();
                try
                {

                    da.Fill(table);
                }
                catch (Exception e)
                {
                    System.IO.File.WriteAllText("D:\\testuseadapter.TXT", e.Message.ToString());
                }
                foreach (DataRow row in table.Rows)
                {
                    var value = Convert.ToString(row["capp_name"]).Trim();
                    var key = Convert.ToString(row["capp_id"]).Trim();
                    KeyValuePair<string, string> entity = new KeyValuePair<string, string>(key, value);
                    result.Add(entity);
                }



            }


            //using (var cmd = SqlHelper.GetFoxConnection1(file).CreateCommand())
            //{
            //    cmd.CommandText = "Select capp_name,capp_id from sydappl order by capp_name";
            //    System.Data.OleDb.OleDbDataAdapter da = new System.Data.OleDb.OleDbDataAdapter(cmd);
            //    DataTable table = new DataTable();
            //    try
            //    {

            //        da.Fill(table);
            //    }
            //    catch (Exception e)
            //    {
            //        System.IO.File.WriteAllText("D:\\testuseadapter.TXT", e.Message.ToString());
            //    }
            //    foreach (DataRow row in table.Rows)
            //    {
            //        var value = Convert.ToString(row["capp_name"]).Trim();
            //        var key = Convert.ToString(row["capp_id"]).Trim();
            //        KeyValuePair<string, string> entity = new KeyValuePair<string, string>(key, value);
            //        result.Add(entity);
            //    }



            //}
            return result;
        }
    }
}