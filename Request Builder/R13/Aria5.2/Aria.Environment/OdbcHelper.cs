using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Odbc;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace Aria.Environment
{
    public class OdbcHelper
    {
        public const string DOT_EXT = ".XML";

        public static void LoadDataTableByConnection(DataTable table, OdbcCommand comm, string key)
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();

                table.Load(comm.ExecuteReader());
                table.TableName = key;

                comm.Connection.Close();
            }
            else
            {
                table.Load(comm.ExecuteReader());
                table.TableName = key;
            }
        }

        public static void LoadDataTable(DataTable table, OdbcCommand comm, string cachePath, string key)
        {
            string file = Path.Combine(cachePath, key + DOT_EXT);

            //heba selim - begin
            ////MMT
            //const string pathm = @"D:\Heba\Example.txt";
            ////MMT
            // 1--get table name 
                // split table from the command  
            string[] tableSplitter = comm.CommandText.Split(new string[] { " " }, StringSplitOptions.None);
            string tableName="";
            for (int i = 0; i < tableSplitter.Length; i++)
            {
                if (tableSplitter[i].ToLower() == "from")
                {
                    //continue;
                    tableName = tableSplitter[++i];
                    break;
                    //Console.Write(tableSplitter[++i]);
                }
            }
            ////MMT

            ////File.Create(pathm);
           // TextWriter tw = new StreamWriter(pathm);
            //tw.WriteLine(tableName);
            ////tw.Close();
            ////MMT
            //2--get the connection
            string dataSource = "";
            string[] tableSplittersrc = comm.Connection.ConnectionString.Split(new string[] { ";","=" }, StringSplitOptions.None);
            
            for (int i = 0; i < tableSplittersrc.Length; i++)
            {
                if (tableSplittersrc[i].ToLower() == "sourcedb")
                {
                    //continue;
                    dataSource = tableSplittersrc[++i];
                    break;
                    //Console.Write(tableSplitter[++i]);
                }
            }
              

             ////MMT
             //tw.WriteLine(dataSource);
             ////tw.Close();
             ////MMT

            //compare modfied date (date modfied date and the xml modified date)
             string path = Path.Combine(dataSource, tableName);

             string pathDbf = path + ".dbf";
             string pathFpt = path + ".fpt";
             string pathCdx = path + ".cdx";
            ////MMT
            // tw.WriteLine(pathDbf);
            // tw.Close();
            ////MMT
             var lastModifiedDbf = System.IO.File.GetLastWriteTime(pathDbf);
             var lastModifiedFpt = System.IO.File.GetLastWriteTime(pathFpt);
             var lastModifiedCdx = System.IO.File.GetLastWriteTime(pathCdx);
             var lastModifiedFile=System.IO.File.GetLastWriteTime(file);
            //test
             //const string pathes = @"D:\Heba\Example.txt";
             //if (!File.Exists(pathes))
             //{
             //    File.Create(pathes);
             //    TextWriter tw = new StreamWriter(pathes);
             //    tw.WriteLine(pathDbf);
             //    tw.Close();
             //}
             //else 
             //{
             //    TextWriter tw = new StreamWriter(pathes);
             //    tw.Write(pathDbf);
             //    tw.Close();
             //}
            //test
             //bool loadFromDB=false;
             //if (File.Exists(pathDbf) || File.Exists(pathFpt))
             //{
             //    if (lastModifiedDbf.Date > lastModifiedFile.Date || lastModifiedCdx.Date > lastModifiedFile.Date || lastModifiedFpt.Date > lastModifiedFile.Date)
             //        loadFromDB = true;
             //}
             //if (File.Exists(file))
             //{
             //    if (loadFromDB)
             //    {
             //        LoadDataTableByConnection(table, comm, key);

             //        try
             //        {
             //            table.WriteXml(file, XmlWriteMode.WriteSchema);
             //        }
             //        catch (Exception)
             //        {
             //        }
             //    }
             //    else
             //        table.ReadXml(file);
             //}
             //else
             //{
             //    LoadDataTableByConnection(table, comm, key);

             //    try
             //    {
             //        table.WriteXml(file, XmlWriteMode.WriteSchema);
             //    }
             //    catch (Exception)
             //    {
             //    }
             //}
           //heba selim - end

             if (File.Exists(file))
             {
                 //heba selim - begin
                  lastModifiedFile = System.IO.File.GetLastWriteTime(file);
                 try
                 {
                     ////MMT
                     //tw.WriteLine("First line in 1st try");
                     ////MMT
                     //MessageBox.Show("in the first try");

                     if (File.Exists(pathDbf) || File.Exists(pathFpt))
                     {
                         if (lastModifiedDbf > lastModifiedFile || lastModifiedCdx > lastModifiedFile || lastModifiedFpt > lastModifiedFile)
                         {
                             //tw.WriteLine("second line in 1st try");

                             LoadDataTableByConnection(table, comm, key);
                             ////MMT
                             //tw.WriteLine("after load datatable in 1st try");
                             ////MMT
                             try
                             {

                                 table.WriteXml(file, XmlWriteMode.WriteSchema);
                                 ////MMT
                                 //tw.WriteLine("after write XML in 2nd try");
                                 ////MMT
                             }
                             catch (Exception)
                             {
                             }
                         }
                         else 
                             table.ReadXml(file);

                     }

                     else
                         table.ReadXml(file);
                 }

                 //heba selim - end
                 catch (Exception)
                 {
                     LoadDataTableByConnection(table, comm, key);

                     try
                     {
                         table.WriteXml(file, XmlWriteMode.WriteSchema);
                     }
                     catch (Exception)
                     {
                     }
                 }
             }
             else
             {
                 LoadDataTableByConnection(table, comm, key);

                 try
                 {
                     //MessageBox.Show("in the last try");

                     table.WriteXml(file, XmlWriteMode.WriteSchema);
                 }
                 catch (Exception)
                 {
                 }
             }
            ////MMT
            // tw.Close();
        }
    }
}
