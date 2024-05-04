using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace updateprojectswebjob
{
    public class addtotable
    {
        public static void func(string script)
        {
             //string sql = "CREATE TABLE d:\\datafile.dbf (ID C(10) , mrepfxflt M(10))";
             System.Data.OleDb.OleDbConnection OleConn = new System.Data.OleDb.OleDbConnection();

            OleConn.ConnectionString = @"Provider=VFPOLEDB.1;Data Source=D:\";
            OleConn.Open();
            //System.Data.OleDb.OleDbCommand OleComm = new System.Data.OleDb.OleDbCommand(sql, OleConn);
           // OleComm.ExecuteNonQuery();


            // System.IO.File.WriteAllText("D:\\mmm.txt", t1);
            //string sqlInsert = "Insert into 'd:\\datafile.dbf' values('XXX',FILETOSTR('D:\\mmm.txt'))";
            string sqlInsert = string.Format(script, "'D:\\sysfiles\\SYDREPRT.dbf'");
            System.Data.OleDb.OleDbCommand OleComm1 = new System.Data.OleDb.OleDbCommand(sqlInsert, OleConn);
            OleComm1.ExecuteNonQuery();
        }
        
    }
}
