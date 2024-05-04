using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Azure.WebJobs;
using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module;
using DevExpress.Data.Filtering;
using System.Data.SqlClient;
using System.IO;
using Excel = Microsoft.Office.Interop.Excel;
using DevExpress.Spreadsheet;
using DevExpress.Web.ASPxSpreadsheet;
using System.Data;
using Ionic.Zip;
using System.Data.OleDb;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using System.Text.RegularExpressions;
using memototext;



namespace updateprojectswebjob
{
    // To learn more about Microsoft Azure WebJobs SDK, please see http://go.microsoft.com/fwlink/?LinkID=320976
    class Program
    {
        // Please set the following connection strings in app.config for this WebJob to run:
        // AzureWebJobsDashboard and AzureWebJobsStorage
        //const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
        //const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin_testing;User ID=sa;Password=aria_123";




        //Belal Ragab
        //connection string local server and database with name Aria5SystemAdmin-HR
        const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";



        public static string ArrayToMemo(Array array, string ArrayName)
        {
            StringBuilder builder = new StringBuilder();
            int num = (array.Rank > 0) ? array.GetLength(0) : 0;
            int num2 = (array.Rank > 1) ? array.GetLength(1) : 0;
            ArrayName = ArrayName.PadRight(11, '\0');
            builder.Append("M" + '\x000f');

            builder.Append(string.Concat(new object[] { ArrayName, "A", new string('\0', 13), '\x0003', new string('\0', 6), (char)num, '\0', (char)num2, '\0' }));
            string format = string.Concat(new object[] { ArrayName, "{0}", new string('\0', 4), "{1}", new string('\0', 15), "{2}", '\0' });
            for (int i = 0; i < num; i++)
            {
                object obj2;
                if (num2 > 0)
                {
                    for (int j = 0; j < num2; j++)
                    {
                        obj2 = array.GetValue(i, j);
                        builder.Append(string.Format(format, GetFoxType(obj2).ToString(), (char)(GetLength(obj2) + 1), GetValue(obj2)));
                        if (obj2 is bool)
                        {
                            builder.Remove(builder.Length - 1, 1);
                        }
                    }
                }
                else
                {
                    obj2 = array.GetValue(i);
                    builder.Append(string.Format(format, GetFoxType(obj2).ToString(), (char)(GetLength(obj2) + 1), GetValue(obj2)));
                }
            }
            builder.Append('\x001a');
            return builder.ToString();
        }

        public static char GetFoxType(object obj)
        {
            if (obj != null)
            {
                string fullName = obj.GetType().FullName;
                if (fullName == typeof(string).FullName)
                {
                    return 'C';
                }
                if (fullName == typeof(bool).FullName)
                {
                    return 'L';
                }
            }
            return 'L';
        }

        public static int GetLength(object obj)
        {
            int num = 0;
            if (obj != null)
            {
                string fullName = obj.GetType().FullName;
                if (fullName == typeof(string).FullName)
                {
                    return Convert.ToString(obj).Length;
                }
                if (fullName == typeof(bool).FullName)
                {
                    num = 1;
                }
            }
            return num;
        }

        public static string GetValue(object obj)
        {
            string str = "";
            if (obj != null)
            {
                string fullName = obj.GetType().FullName;
                if (fullName == typeof(bool).FullName)
                {
                    return (((bool)obj) ? new string('\x0001', 1) : new string('\0', 1));
                }
                if (fullName == typeof(string).FullName)
                {
                    str = Convert.ToString(obj);
                }
            }
            return str;
        }


        //step 1
        static void Main()
        {

            // sara [start]
            //Run For frist time only 
            addobjecttypes();
            updateallobjecttype();
            //Menu
            insertMenuSetting();
            //index
            insertIndexsetting();
            //Field
            insertFieldTypesetting();
            //Other types
            insertTypesetting();
            //File
            insertFileTypesetting();
            //D:\sydindexfields(AriaIndex).xlsx
            //DODO
            //DODO
            //return;
            ////sara[End]

            ////DateTime da = CalcEndate.CalcEnddate(new DateTime(2017, 8, 4), 1);
            ////DateTime x = da.AddDays(1);
            ////string c = "EXE FILE";
            ////string c2 = c.Trim();

            //var host = new JobHost();

            uploadsysfilesrecords("SYDOBJCT.DBF", "Capp_id", "D:\\Tracking-Test-Folder\\ObjectsMemofields\\", "Capobjnam", "Ariaobject", "cprglname");
            uploadsysfilesrecords("SYDFILES.dbf", "Cfile_nam", "D:\\Tracking-Test-Folder\\FielsMEMOFIELDS\\", "", "Ariafile", "");
            uploadsysfilesrecords("SYCTRIGG.DBF", "Capobjnam", "D:\\Tracking-Test-Folder\\syctrigger meofields\\", "", "Ariatrigger", "");
            uploadsysfilesrecords("SYDREPRT.DBF", "crep_name", "D:\\Tracking-Test-Folder\\ReportMemofields\\", "", "Ariareport", "crep_name");
            uploadsysfilesrecords("SYCMENU.DBF", "Capp_id", "D:\\Tracking-Test-Folder\\Menu\\", "Csub_prpt", "Menu", "");
            uploadsysfilesrecords("SYDINDEX.DBF", "Cfile_tag", "D:\\Tracking-Test-Folder\\INDEXMEMOFIELDS\\", "", "Ariaindex", "");
            uploadsysfilesrecords("SYDFIELD.dbf", "Cfld_name", "D:\\Tracking-Test-Folder\\AriaField\\", "", "AriaField", "");

            //Run For frist time only 
            MapsysfilesToentities();
            LinkFILEFIELD();
            MapIndexToentities();

            //string x = "sigments' ahmed";
            //x = x.Replace("'", "''");
            //Run For frist time only 
            string filepath = @"D:\Aria4xpObjectswithmodifiedentity.xlsx";
            InsertAriaObjects(filepath, "Aria4xp");
            filepath = @"D:\EDImetadatawithnewentitynames.xlsx";
            InsertAriaObjects(filepath, "Aria3EDI");
        }

        public static void updateallobjecttype()
        {
            Session sess = new Session();
            sess.ConnectionString = connectionstr;
            sess.Connect();

            System.Collections.ICollection list = sess.GetObjects(sess.Dictionary.GetClassInfo(typeof(ObjectType)), CriteriaOperator.Parse("[Name] != 'NULL'"), null, 1000, false, false);
            IList<ObjectType> objectslist = list.Cast<ObjectType>().ToList();

            foreach (ObjectType item in objectslist)
            {
                if (item.SettingTypes.FirstOrDefault(x => x.SettingTypeId == "StorageFileName") == null)
                {
                    SettingType storagefilename = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'StorageFileName'"));
                    if (storagefilename != null)
                    {
                        item.SettingTypes.Add(storagefilename);
                        item.Save();
                    }
                }
            }
        }
        public static void InsertAriaObjects(string excelPath, string APPID)
        {
            #region filldec
            Dictionary<string, string> typesdic = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            typesdic.Add(".APP", "Application          ");
            typesdic.Add(".bat", "BatchFile           ");
            typesdic.Add(".bmp", "Image                ");
            typesdic.Add(".cdx", "TableIndexfile     ");
            typesdic.Add(".chm", "Helpfile            ");
            typesdic.Add(".ctl", "CTLFile             ");
            typesdic.Add(".ctx", "CTXFile             ");
            typesdic.Add(".cur", "Image                ");
            typesdic.Add(".DBF", "Table                ");
            typesdic.Add(".dll", "DLL                  ");
            typesdic.Add(".exe", "EXEFile             ");
            typesdic.Add(".fll", "FLLFile             ");
            // typesdic.Add(".FPT","Table Memo File      ");
            typesdic.Add(".FPW", "Configurationfile   ");
            // typesdic.Add(".FRT","Report               ");
            typesdic.Add(".frx", "Report               ");
            typesdic.Add(".FXP", "Program              ");
            typesdic.Add(".gif", "Image                ");
            typesdic.Add(".h", "HeaderFile        ");
            typesdic.Add(".HLP", "helpfile            ");
            typesdic.Add(".ico", "Icon                 ");
            typesdic.Add(".ism", "ISMFile             ");
            typesdic.Add(".jpg", "Image                ");
            // typesdic.Add(".lbt","Label                ");
            typesdic.Add(".lbx", "Label                ");
            typesdic.Add(".lib", "Library              ");
            typesdic.Add(".log", "Logfile             ");
            typesdic.Add(".MEM", "MemFile             ");
            typesdic.Add(".msi", "InstallationFile    ");
            typesdic.Add(".msk", "MSKFILE             ");
            typesdic.Add(".oca", "OCAFile             ");
            typesdic.Add(".OCX", "OCXFile             ");
            typesdic.Add(".PJT", "Project              ");
            typesdic.Add(".pjx", "Project              ");
            typesdic.Add(".png", "Image                ");
            typesdic.Add(".prg", "Program              ");
            typesdic.Add(".rar", "Rar File             ");
            typesdic.Add(".rpt", "CrystalReport       ");
            // typesdic.Add(".SCT","Screen               ");
            typesdic.Add(".scx", "Screen               ");
            typesdic.Add(".tlb", "TypeLibrary         ");
            typesdic.Add(".txt", "Text                 ");
            typesdic.Add(".vbg", "VisualBasic         ");
            typesdic.Add(".vbp", "VisualBasic         ");
            typesdic.Add(".VBR", "VisualBasic         ");
            typesdic.Add(".vbw", "VisualBasic         ");
            // typesdic.Add(".VCT","Class                ");
            typesdic.Add(".vcx", "Class                ");
            typesdic.Add(".xls", "Excel                ");
            typesdic.Add(".xml", "XMLFile             ");
            typesdic.Add(".zip", "ZIPFile             ");
            #endregion
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(excelPath));
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 3; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                ObjectType entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Entity'"));
                Application_T App = sess.FindObject<Application_T>(CriteriaOperator.Parse("[Id] = '" + APPID + "'"));
                foreach (DataRow item in dataTable.Rows)
                {
                    string objectname = item[0].ToString();
                    if (!typesdic.Keys.Contains(Path.GetExtension(objectname).ToUpper()))
                    {
                        continue;
                    }
                    switch (Path.GetExtension(objectname).ToUpper())
                    {
                        case ".SCT":
                            objectname = objectname.Replace(".SCT", ".SCX");
                            break;
                        case ".FRT":
                            objectname = objectname.Replace(".FRT", ".FRX");
                            break;
                        case ".VCT":
                            objectname = objectname.Replace(".VCT", ".VCX");
                            break;
                        case ".FPT":
                            objectname = objectname.Replace(".FPT", ".DBF");
                            break;
                        case ".CDX":
                            objectname = objectname.Replace(".CDX", ".DBF");

                            break;
                        case ".LBT":
                            objectname = objectname.Replace(".LBT", ".LBX");
                            break;
                        case ".FXP":
                            objectname = objectname.Replace(".FXP", ".PRG");
                            break;
                    }
                    string objectpath = item[1].ToString();
                    string objectentity = item[2].ToString();
                    objectentity = objectentity.Replace("'", "''");

                    string typeid = typesdic[Path.GetExtension(objectname).ToUpper()].Trim().ToUpper();
                    ObjectType objecttype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("UPPER([ObjectTypeID]) = '" + typeid.Trim() + "'"));
                    AriaObject Parentobject = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectName] = '" + objectentity + "' and [Application] = '" + App.Oid + "'"));
                    if (Parentobject == null)
                    {

                        Parentobject = new AriaObject(sess);
                        Parentobject.ObjectType = entitytype;
                        Parentobject.ObjectName = objectentity;
                        Parentobject.Applications.Add(App);
                        Parentobject.Application = App;
                        Parentobject.ConversionDate = DateTime.Now;

                        Parentobject.Save();
                    }
                    //  Application_T App = sess.FindObject<Application_T>(CriteriaOperator.Parse("[Id] = 'Aria4XP'"));
                    AriaObject existbefor = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectName]='" + objectname.Trim().Replace("'", "''") + "' and [Application] = '" + App.Oid + "'"));
                    if (objecttype != null )
                    {
                        if (existbefor == null)
                        {
                            AriaObject newobject = new AriaObject(sess);
                            newobject.ObjectType = objecttype;
                            newobject.ObjectName = objectname;
                            newobject.Application = App;
                            newobject.ParentObjectID = Parentobject;
                            newobject.ConversionDate = DateTime.Now;
                            newobject.Save();
                            AriaObjectSetting subfoldersetting = newobject.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "SubFolder");
                            if (subfoldersetting != null)
                            {
                                subfoldersetting.Value = objectpath;
                                subfoldersetting.DataType = "CHARACTER";
                                subfoldersetting.Width = 50;
                                //Doaa
                                subfoldersetting.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                //Doaa
                                subfoldersetting.Save();
                            }
                            else
                            {
                                AriaObjectSetting subfoldersetting1 = new AriaObjectSetting(sess);
                                subfoldersetting1.SettingType = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'SubFolder'"));
                                // AriaObjectSetting subfoldersetting1 = newobject.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "Storage File Name");
                                subfoldersetting1.Value = objectpath;
                                subfoldersetting1.DataType = "CHARACTER";
                                subfoldersetting1.Width = 50;
                                //Doaa
                                subfoldersetting1.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                subfoldersetting1.Save();
                                //Doaa

                                newobject.AriaObjectSettings.Add(subfoldersetting1);
                                newobject.ConversionDate = DateTime.Now;

                                newobject.Save();
                            }
                            AriaObjectSetting storagefilesetting = newobject.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "StorageFileName");
                            if (storagefilesetting != null)
                            {
                                storagefilesetting.Value = objectname;
                                storagefilesetting.DataType = "CHARACTER";
                                storagefilesetting.Width = 50;
                                //Doaa
                                storagefilesetting.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                //Doaa

                                storagefilesetting.Save();
                            }
                            else
                            {
                                AriaObjectSetting storagefilenamesetting = new AriaObjectSetting(sess);
                                storagefilenamesetting.SettingType = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'StorageFileName'"));
                                // AriaObjectSetting subfoldersetting1 = newobject.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "Storage File Name");
                                storagefilenamesetting.Value = objectname;
                                storagefilenamesetting.DataType = "CHARACTER";
                                storagefilenamesetting.Width = 50;
                                //Doaa
                                storagefilenamesetting.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                storagefilenamesetting.Save();
                                //Doaa
                                newobject.AriaObjectSettings.Add(storagefilenamesetting);
                                newobject.ConversionDate = DateTime.Now;

                                newobject.Save();
                            }
                        }
                        else
                        {
                            existbefor.ConversionDate = DateTime.Now;

                            existbefor.Save();
                             AriaObjectSetting subfoldersetting = existbefor.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "SubFolder");

                            if (subfoldersetting != null)
                            {
                                subfoldersetting.Value = objectpath;
                                //Doaa
                                subfoldersetting.DataType = "CHARACTER";
                                subfoldersetting.Width = 50;

                                subfoldersetting.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                //Doaa
                                subfoldersetting.Save();
                            }
                            else
                            {
                                AriaObjectSetting subfoldersetting1 = new AriaObjectSetting(sess);
                                subfoldersetting1.SettingType = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'SubFolder'"));
                                //subfoldersetting1.AriaObjectRevision = sess.FindObject<AriaObjectRevision>(CriteriaOperator.Parse("[SettingTypeId] = 'SubFolder'"));
                                // AriaObjectSetting subfoldersetting1 = newobject.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "Storage File Name");
                                subfoldersetting1.Value = objectpath;
                                subfoldersetting1.DataType = "CHARACTER";
                                subfoldersetting1.Width = 50;

                                subfoldersetting1.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault () ;
                                subfoldersetting1.Save();
                                existbefor.AriaObjectSettings.Add(subfoldersetting1);
                                existbefor.ConversionDate = DateTime.Now;

                                existbefor.Save();
                            }
                            AriaObjectSetting storagefilesetting = existbefor.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "StorageFileName");
                            if (storagefilesetting != null)
                            {
                                storagefilesetting.Value = objectname;
                                //Doaa
                                storagefilesetting.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                storagefilesetting.DataType = "CHARACTER";
                                storagefilesetting.Width = 50;

                                //Doaa
                                storagefilesetting.Save();
                            }
                            else
                            {
                                AriaObjectSetting storagefilenamesetting = new AriaObjectSetting(sess);
                                storagefilenamesetting.SettingType = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'StorageFileName'"));
                                // AriaObjectSetting subfoldersetting1 = newobject.AriaObjectSettings.FirstOrDefault(x => x.SettingType.SettingTypeId == "Storage File Name");
                                storagefilenamesetting.Value = objectname;
                                //Doaa
                                storagefilenamesetting.DataType = "CHARACTER";
                                storagefilenamesetting.Width = 50;
                                storagefilenamesetting.AriaObjectRevision = existbefor.AriaObjectRevisions.FirstOrDefault();
                                storagefilenamesetting.Save();
                                //Doaa
                                existbefor.AriaObjectSettings.Add(storagefilenamesetting);
                                existbefor.ConversionDate = DateTime.Now;

                                existbefor.Save();
                            }
                        }
                    }
                   

                }
            }
        }

        public static void MapsysfilesToentities()
        {
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\Aria4xpTablesentities.xlsx"));
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            DataTable dataTable = new DataTable();
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 3; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                ObjectType entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Entity'"));
                ObjectType Filetype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Ariafile'"));

                foreach (DataRow datarow in dataTable.Rows)
                {
                    AriaObject Entity = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectType] = '" + entitytype.Oid + "' and [ObjectName]= '" + datarow[1].ToString() + "'"));
                    if (Entity != null)
                    {

                        AriaObject Filerecord = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectType] = '" + Filetype.Oid + "' and [ObjectName]= '" + datarow[0].ToString() + "'"));
                        if (Filerecord != null)
                        {
                            Filerecord.ParentObjectID = Entity;
                            Filerecord.Save();
                        }
                    }
                }
            }

        }
        public static void addobjecttypes()
        {
            string[] types = new string[]
            {
   "Application"                 ,
   "Batch File"                  ,
   " Configuration file    "      ,
   " Class                 "      ,
   " Crystal Report        "      ,
   " CTL File              "      ,
   " CTX File              "      ,
   " DLL                   "      ,
   " Excel                 "      ,
   " EXE File              "      ,
   " FLL File              "      ,
   " Header File           "      ,
   " Help file             "      ,
   " Icon                  "      ,
   " Image                 "      ,
   " Installation File     "      ,
   " ISM File              "      ,
   " Label                 "      ,
   " Library               "      ,
   " Log file              "      ,
   " Mem File              "      ,
   " MSK FILE              "      ,
   " OCA File              "      ,
   " OCX File              "      ,
   " Program               "      ,
   " Project               "      ,
   " Rar File              "      ,
   " Report                "      ,
   " Screen                "      ,
   " Table                 "      ,
   " Table Index file      "      ,
   " Table Memo File       "      ,
   " Text                  "      ,
   " Type Library          "      ,
   " Visual Basic          "      ,
   " XML File              "      ,
   " ZIP File              "
};
            Session sess = new Session();
            sess.ConnectionString = connectionstr;
            sess.Connect();
            SettingType subfolder = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'SubFolder'"));
            SettingType subsystem = sess.FindObject<SettingType>(CriteriaOperator.Parse("[SettingTypeId] = 'StorageFileName'"));
            foreach (string item in types)
            {
                ObjectType entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = '" + item.Trim() + "'"));
                if (entitytype == null)
                {
                    ObjectType newobjecttype = new ObjectType(sess);
                    newobjecttype.ObjectTypeID = item.Trim();
                    newobjecttype.Name = item.Trim();
                    newobjecttype.SettingTypes.Add(subfolder);
                    newobjecttype.SettingTypes.Add(subsystem);
                    newobjecttype.Save();
                }

            }
        }
        public static void insertAriaobjectsEntities()
        {
            DataTable Entities = new DataTable();
            OleDbConnection yourConnectionHandler = new OleDbConnection(
               @"Provider=VFPOLEDB.1;Data Source=D:\ARIA4XP\Aria4xp (R13)\\sysfiles\\");

            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from  SYDObjct.dbf";  // dbf table name

            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(Entities);
            if (Entities.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                ObjectType entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Entity'"));
                Application_T App = sess.FindObject<Application_T>(CriteriaOperator.Parse("[Id] = 'Aria4XP'"));
                if (entitytype != null)
                {
                    foreach (DataRow record in Entities.Rows)
                    {



                        AriaObject newobject = new AriaObject(sess);
                        newobject.ObjectType = entitytype;
                        newobject.ObjectName = record["Cprglname"].ToString();
                        newobject.Applications.Add(App);
                        newobject.Application = App;
                        newobject.Save();
                    }
                }
            }
        }

        public static void insertAriaReportEntities()
        {
            DataTable Entities = new DataTable();
            OleDbConnection yourConnectionHandler = new OleDbConnection(
               @"Provider=VFPOLEDB.1;Data Source=D:\ARIA4XP\Aria4xp (R13)\\sysfiles\\");

            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from  SYDREPRT.dbf";  // dbf table name

            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(Entities);
            if (Entities.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                ObjectType entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Entity'"));
                Application_T App = sess.FindObject<Application_T>(CriteriaOperator.Parse("[Id] = 'Aria4XP'"));
                if (entitytype != null)
                {
                    foreach (DataRow record in Entities.Rows)
                    {

                        AriaObject newobject = new AriaObject(sess);
                        newobject.ObjectType = entitytype;
                        newobject.ObjectName = record["Crep_name"].ToString();
                        newobject.Applications.Add(App);
                        newobject.Application = App;
                        newobject.Save();
                    }
                }
            }
        }
        public static void insertTypesetting()
        {
            // byte[] xxxx = File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx");
            //
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx"));
            //
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaObjectsettings(Sydobjects).xlsx"));
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFile(Data)SeetingTypes.xlsx"));
            // 
            //
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 5; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                //
                //ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaField'"));
                ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaObject'"));
                //
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {
                    if (type != null)
                    {
                        SettingType seetingtype = type.SettingTypes.Where(x => x.SettingTypeId.ToUpper () == dataTable.Rows[i][0].ToString().ToUpper ()).FirstOrDefault();
                        if (seetingtype != null)
                        {
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                        }
                        else
                        {
                            seetingtype = new SettingType(sess);
                            seetingtype.SettingTypeId = dataTable.Rows[i][0].ToString().Trim();
                            seetingtype.Name = dataTable.Rows[i][3].ToString();
                            seetingtype.DataType = (SettingType.EnumDataType)Enum.Parse(typeof(SettingType.EnumDataType), dataTable.Rows[i][1].ToString());
                            seetingtype.Width = short.Parse(dataTable.Rows[i][2].ToString());
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                            seetingtype.ObjectTypes.Add(type);
                        }
                        seetingtype.Save();
                    }

                }
                //sess.CommitTransaction();
            }
        }
        public static void insertFileTypesetting()
        {
            // byte[] xxxx = File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx");
            //
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx"));
            //
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaObjectsettings(Sydobjects).xlsx"));
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFile(Data)SeetingTypes.xlsx"));
            // 
            //
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 5; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                //
                //ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaField'"));
                ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaFile'"));
                //
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {
                    if (type != null)
                    {
                        SettingType seetingtype = type.SettingTypes.Where(x => x.SettingTypeId.ToUpper() == dataTable.Rows[i][0].ToString().ToUpper()).FirstOrDefault();
                        if (seetingtype != null)
                        {
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                        }
                        else
                        {
                            seetingtype = new SettingType(sess);
                            seetingtype.SettingTypeId = dataTable.Rows[i][0].ToString().Trim();
                            seetingtype.Name = dataTable.Rows[i][1].ToString();
                            seetingtype.DataType = (SettingType.EnumDataType)Enum.Parse(typeof(SettingType.EnumDataType), dataTable.Rows[i][2].ToString());
                            seetingtype.Width = short.Parse(dataTable.Rows[i][3].ToString());
                            seetingtype.ActualColumnName = dataTable.Rows[i][4].ToString();
                            seetingtype.ObjectTypes.Add(type);
                        }
                        seetingtype.Save();
                    }

                }
                //sess.CommitTransaction();
            }
        }
        public static void insertFieldTypesetting()
        {
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx"));
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 5; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaField'"));
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {
                    if (type != null)
                    {
                        SettingType seetingtype = type.SettingTypes.Where(x => x.SettingTypeId == dataTable.Rows[i][4].ToString()).FirstOrDefault();
                        if (seetingtype != null)
                        {
                            seetingtype.ActualColumnName = dataTable.Rows[i][4].ToString();
                        }
                        else
                        {
                            seetingtype = new SettingType(sess);
                            seetingtype.SettingTypeId = dataTable.Rows[i][0].ToString().Trim();
                            seetingtype.Name = dataTable.Rows[i][1].ToString();
                            seetingtype.DataType = (SettingType.EnumDataType)Enum.Parse(typeof(SettingType.EnumDataType), dataTable.Rows[i][2].ToString());
                            seetingtype.Width = short.Parse(dataTable.Rows[i][3].ToString());
                            seetingtype.ActualColumnName = dataTable.Rows[i][4].ToString();
                            seetingtype.ObjectTypes.Add(type);
                        }
                        seetingtype.Save();
                    }

                }
                //sess.CommitTransaction();
            }
        }
        public void insertemployee()
        {
            #region insert into system
            byte[] xxxx = System.IO.File.ReadAllBytes(@"D:\Employees.xlsx");
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\Employees.xlsx"));
            #region uploadmetadata
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 28; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();

                #region create job position code commented
                //HRJobPosition Position = null;
                //  for (int i = 4; i < dataTable.Rows.Count; i++)
                //  {


                //      Position = new HRJobPosition(sess);
                //      HRJobPosition parentposition = sess.FindObject<HRJobPosition>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[i][5].ToString() + "'"));
                //      Department dept = sess.FindObject<Department>(CriteriaOperator.Parse("Id = '"+dataTable.Rows[i][2].ToString()+"'"));
                //      Position.Id = dataTable.Rows[i][0].ToString();
                //      Position.Name = dataTable.Rows[i][1].ToString();
                //      Position.Department = dept;
                //      //this is working days dataTable.Rows[i][3].ToString()
                //      if (dataTable.Rows[i][2].ToString() == "Client Services ")
                //      {
                //          Position.Mon = true;
                //          Position.Tue = true;
                //          Position.Wed = true;
                //          Position.Thu = true;
                //          Position.Fri = true;
                //          //Position.WorkingHoursFrom = TimeSpan.Parse(dataTable.Rows[i][3].ToString()).Add(new TimeSpan(0, 30, 0));

                //          //Position.WorkingHoursTo = TimeSpan.Parse(dataTable.Rows[i][4].ToString());
                //          Position.WorkingHoursFrom = new TimeSpan(14, 30, 0);

                //          Position.WorkingHoursTo = new TimeSpan(23, 00, 0); 
                //      }
                //      else
                //      {
                //          Position.Mon = true;
                //          Position.Tue = true;
                //          Position.Wed = true;
                //          Position.Thu = true;
                //          Position.Sun = true;
                //      //    Position.WorkingHoursFrom = TimeSpan.Parse(dataTable.Rows[i][3].ToString());

                //      //    Position.WorkingHoursTo = TimeSpan.Parse(dataTable.Rows[i][4].ToString()).Add(new TimeSpan(0, 30, 0));

                //          Position.WorkingHoursFrom = new TimeSpan(9,0, 0);
                //          Position.WorkingHoursTo = new TimeSpan(17, 30, 0);

                //      }
                //      Position.ReportTo1 = parentposition;
                //      Position.JobDescription = dataTable.Rows[i][6].ToString();


                //      Position.Save();
                #endregion

                #region create employee
                HREmployee Employee = null;
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {


                    Employee = new HREmployee(sess);
                    HRJobPosition position = sess.FindObject<HRJobPosition>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[i][18].ToString() + "'"));
                    Department dept = sess.FindObject<Department>(CriteriaOperator.Parse("Id = '" + dataTable.Rows[i][17].ToString() + "'"));
                    DevExpress.Persistent.BaseImpl.Country nationality = sess.FindObject<DevExpress.Persistent.BaseImpl.Country>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[i][9].ToString() + "'"));
                    Language lang = sess.FindObject<Language>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[i][11].ToString() + "'"));

                    Employee.Id = long.Parse(dataTable.Rows[i][0].ToString()).ToString("000");
                    Employee.TitleOfCourtesy = (TitleOfCourtesy)Enum.Parse(typeof(TitleOfCourtesy), dataTable.Rows[i][1].ToString());
                    Employee.FirstName = dataTable.Rows[i][2].ToString();
                    Employee.MiddleName = dataTable.Rows[i][3].ToString();
                    Employee.LastName = dataTable.Rows[i][4].ToString();
                    Employee.NickName = dataTable.Rows[i][5].ToString();
                    Employee.Gender = (Gender)Enum.Parse(typeof(Gender), dataTable.Rows[i][6].ToString());
                    //dataTable.Rows[i][7].ToString()   dh el branch 
                    Employee.BirthDate = DateTime.Parse(dataTable.Rows[i][8].ToString()).Date;
                    //dataTable.Rows[i][9].ToString()   dh el nationality 
                    Employee.Nationality = nationality;
                    Employee.SSN = dataTable.Rows[i][10].ToString();
                    //dataTable.Rows[i][11].ToString() del l languagh 
                    Employee.Language = lang;
                    Employee.EMailAddress = dataTable.Rows[i][12].ToString();
                    Employee.WebPageAddress = dataTable.Rows[i][13].ToString();
                    Employee.EmploymentStatus = (Employment_status)Enum.Parse(typeof(Employment_status), dataTable.Rows[i][14].ToString());
                    Employee.EmploymentDate = DateTime.Parse(dataTable.Rows[i][15].ToString()).Date;
                    Employee.EmploymentType = (Employment_Type)Enum.Parse(typeof(Employment_Type), dataTable.Rows[i][16].ToString());
                    // dataTable.Rows[i][17].ToString() department
                    Employee.Department = dept;
                    // dataTable.Rows[i][18].ToString() dh el position 
                    Employee.JobPosition = position;

                    if (((insurance_status)Enum.Parse(typeof(insurance_status), dataTable.Rows[i][19].ToString())) == insurance_status.Active)
                    {
                        Employee.SocialInsuranceStatus = insurance_status.Active;
                        Employee.SocialInsuranceNo = dataTable.Rows[i][20].ToString();
                    }
                    if (((insurance_status)Enum.Parse(typeof(insurance_status), dataTable.Rows[i][21].ToString())) == insurance_status.Active)
                    {
                        Employee.MedicalInsuranceStatus = insurance_status.Active;
                        Employee.MedicalInsuranceNo = dataTable.Rows[i][22].ToString();
                        Employee.MedicalInsuranceStartDate = DateTime.Parse(dataTable.Rows[i][23].ToString()).Date;
                    }
                    Employee.MaritalStatus = (Marital_status)Enum.Parse(typeof(Marital_status), dataTable.Rows[i][24].ToString());
                    Employee.Save();
                    #endregion
                    // sess.CommitTransaction();

                }
                sess.Disconnect();
            }


            #endregion
            #endregion

        }


        private static string convertAsciiTextToHex(String i_asciiText)
        {
            StringBuilder sBuffer = new StringBuilder();
            for (int i = 0; i < i_asciiText.Length; i++)
            {
                sBuffer.Append(Convert.ToInt32(i_asciiText[i]).ToString("x2"));
            }
            return sBuffer.ToString().ToUpper();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="tablenamewithext"></param>
        /// <param name="namecolumn"></param>
        /// <param name="memofieldspath"></param>
        /// <param name="secondcolumnname"></param>
        /// <param name="typeid"></param>
        /// <param name="originalnamecolumn"></param>
        public static void uploadsysfilesrecords(string tablenamewithext, string namecolumn, string memofieldspath, string secondcolumnname, string typeid, string originalnamecolumn)
        {
            #region open fox dbf and select all data from tablenamewithext
            DataTable Entities = new DataTable();
            string SysfilePath = @"D:\Aria4xp\Aria4xp (R13)\sysfiles\\";
            OleDbConnection yourConnectionHandler = new OleDbConnection("Provider=VFPOLEDB.1;Data Source=" + SysfilePath);
            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from  " + tablenamewithext + "";  // dbf table name
            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(Entities);
            #endregion

            #region open System Admin db and get data from tables 
            //table Application_T where id = Aria4XP
            //table ObjectType where ObjectTypeID = typeid (param)
            //table ObjectType where ObjectTypeID = Entity 

            Session sess = new Session();
            sess.ConnectionString = connectionstr;
            sess.Connect();
            Application_T App = sess.FindObject<Application_T>(CriteriaOperator.Parse("[Id] = 'Aria4XP'"));
            ObjectType otype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = '" + typeid + "'"));
            ObjectType Entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Entity'"));
            #endregion

            #region Create new excel workbook with new worksheet 
            Excel.Application xlApp = new Microsoft.Office.Interop.Excel.Application();
            Excel.Workbook xlWorkBook;
            Excel.Worksheet xlWorkSheet;
            object misValue = System.Reflection.Missing.Value;
            xlWorkBook = xlApp.Workbooks.Add(misValue);
            xlWorkSheet = (Excel.Worksheet)xlWorkBook.Worksheets.get_Item(1);
            #endregion

            int seq = 1;
            int row = 0;

            //loop on data selected from fox db according to the table name(function input param)
            foreach (DataRow item in Entities.Rows)
            {
                row++;
                AriaObject newobject = new AriaObject(sess);
                AriaObject Parentobject = new AriaObject(sess);
                //#objname
                //get object name value[columnname] or (value[columnname] - value[secondcolumnname]) if the second one is not null 
                string Cver = string.Empty;
                //MMT
                string CFILE_NAM = string.Empty;
                //MMT
                string WhereCondetion = string.Empty;
                string name = "";
                if (string.IsNullOrEmpty(secondcolumnname))
                    name = item[namecolumn].ToString();
                else
                    name = item[namecolumn].ToString() + "-" + item[secondcolumnname].ToString();

                WhereCondetion = "[ObjectName] = '" + name.Trim().Replace("'", "''") + "'";
                //MMT
                if (tablenamewithext.ToUpper() == "SYDFIELD.dbf".ToUpper ())
                {
                    WhereCondetion += "AND [ObjectType] ='c694cc14-e771-4cd2-b810-17c49bb9bd64'"; 
                }
                if (tablenamewithext.ToUpper() == "SYDFILES.dbf".ToUpper())
                {
                    WhereCondetion += "AND [ObjectType] ='decd3db9-db6d-48a8-8ef5-0f4b0f717d16' ";
                }
                
                if (tablenamewithext.ToUpper() == "SYDINDEX.dbf".ToUpper())
                {
                    WhereCondetion += "AND [ObjectType] ='C3609FC5-7320-4C18-9B9F-F5F510C196C2'";
                }

                if (tablenamewithext.ToUpper() == "SYDOBJCT.dbf".ToUpper())
                {
                    WhereCondetion += "AND [ObjectType] ='672C3AB5-1919-4F07-8B0B-9A6D1CF7788E'";
                }


                if (tablenamewithext.ToUpper() == "SYDREPRT.dbf".ToUpper())
                {
                    WhereCondetion += "AND [ObjectType] ='2A4A68BC-0722-4044-84D5-93FB3D0D21AD' AND isnull([ObjectDescription],'X') != 'X'";
                }
                //MMT
                AriaObject existing = sess.FindObject<AriaObject>(CriteriaOperator.Parse(WhereCondetion));


                if (tablenamewithext.ToUpper() == "SYDFIELD.dbf".ToUpper() && existing != null)
                {
                    Cver = item["Cver"].ToString();
                    if (!string.IsNullOrEmpty(Cver.Trim()))
                    {
                        var settingrevision = existing.AriaObjectRevisions.Where(sety => sety.AriaObjectSettings.Where(ser => ser.Value == Cver && ser.ActualColumnName.ToUpper().Trim() == "CVER").FirstOrDefault() != null);
                        if (settingrevision.FirstOrDefault() == null)
                        {
                            //Belal
                            //existing.AddEmptyRevision();
                            existing.Save();
                            existing = null;
                        }
                    }
                }

                //MMT
                if (tablenamewithext.ToUpper() == "SYDINDEX.dbf".ToUpper() && existing != null)
                {
                    CFILE_NAM = item["CFILE_NAM"].ToString();
                    if (!string.IsNullOrEmpty(CFILE_NAM.Trim()))
                    {
                        var settingrevision = existing.AriaObjectRevisions.Where(sety => sety.AriaObjectSettings.Where(ser => ser.Value == CFILE_NAM && ser.ActualColumnName.ToUpper().Trim() == "CFILE_NAM").FirstOrDefault() != null);
                        if (settingrevision.FirstOrDefault() == null)
                        {
                            //Belal
                            //existing.AddEmptyRevision();
                            existing.Save();
                            existing = null;
                        }
                    }
                }
                if (tablenamewithext.ToUpper() == "SYCMENU.dbf".ToUpper() && existing != null)
                {
                    string cpross_id = string.Empty;
                    cpross_id = item["cpross_id"].ToString();
                    if (!string.IsNullOrEmpty(cpross_id.Trim()))
                    {
                        var settingrevision = existing.AriaObjectRevisions.Where(sety => sety.AriaObjectSettings.Where(ser => ser.Value == cpross_id && ser.ActualColumnName.ToLower().Trim() == "cpross_id").FirstOrDefault() != null);
                        if (settingrevision.FirstOrDefault() == null)
                        {
                            //Belal
                            //existing.AddEmptyRevision();
                            existing.Save();
                            existing = null;
                        }
                    }
                }
                if (tablenamewithext.ToUpper() == "SYDREPRT.dbf".ToUpper() && existing != null)
                {
                    string crep_id = string.Empty;
                    crep_id = item["crep_id"].ToString();
                    if (!string.IsNullOrEmpty(crep_id.Trim()))
                    {
                        var settingrevision = existing.AriaObjectRevisions.Where(sety => sety.AriaObjectSettings.Where(ser => ser.Value != null &&  ser.Value == crep_id && ser.ActualColumnName != null && ser.ActualColumnName.ToLower().Trim() == "crep_id").FirstOrDefault() != null);
                        if (settingrevision.FirstOrDefault() == null)
                        {
                            //Belal
                            //existing.AddEmptyRevision();
                            existing.Save();
                            existing = null;
                        }
                    }
                }
                //MMT



                //select from table Ariaobjects where objectname = (#objname) 



                #region Update the data in the table AriaObject
                //if (existing != null)
                //{
                //    if (!string.IsNullOrEmpty(originalnamecolumn))
                //    {
                //        // CriteriaOperator cr = new FunctionOperator(FunctionOperatorType.Contains, new OperandProperty("ObjectName"), item[originalnamecolumn].ToString().Trim());
                //        // new FunctionOperator(FunctionOperatorType.Contains, new OperandProperty("ObjectName"), item[originalnamecolumn].ToString().Trim())



                //        //remove the spaces from value[originalnamecolumn]
                //        string parentobjectname = item[originalnamecolumn].ToString().Trim().Replace(" ", "").Replace("'", "''");

                //        //select from aria object in sql db all object that end with the originalnamecolumn (in param) and objecttype is an entity 
                //        AriaObject Parentobject = sess.FindObject<AriaObject>(CriteriaOperator.Parse("EndsWith(ObjectName,'" + parentobjectname + "') and [ObjectType]= '" + Entitytype.Oid + "'"));
                //        //GroupOperator.Combine(GroupOperatorType.And, CriteriaOperator.Parse("[ObjectType].Name == 'Entity'",new FunctionOperator(FunctionOperatorType.Contains, new OperandProperty("ObjectName"),parentobjectname))));// && [ObjectName] like '%" + item[originalnamecolumn].ToString().Trim() +"'"));
                //        if (Parentobject != null)
                //        {
                //            existing.ParentObjectID = Parentobject;
                //            existing.Save();
                //        }
                //    }
                //    continue;
                //}
                #endregion

                if (!string.IsNullOrEmpty(originalnamecolumn) || tablenamewithext.ToUpper() == "SYDFIELD.dbf".ToUpper() || tablenamewithext.ToUpper() == "SYCMENU.dbf".ToUpper())
                {
                    // CriteriaOperator cr = new FunctionOperator(FunctionOperatorType.Contains, new OperandProperty("ObjectName"), item[originalnamecolumn].ToString().Trim());
                    // new FunctionOperator(FunctionOperatorType.Contains, new OperandProperty("ObjectName"), item[originalnamecolumn].ToString().Trim())
                    //remove the spaces from value[originalnamecolumn]

                    string parentobjectname = "";
                    if (tablenamewithext.ToUpper() != "SYDFIELD.dbf".ToUpper() && tablenamewithext.ToUpper() != "SYCMENU.DBF".ToUpper())
                        parentobjectname = item[originalnamecolumn].ToString().Trim().Replace(" ", "").Replace("'", "''");
                    else
                        parentobjectname = "Aria4MainSystem";
                    if (tablenamewithext.ToUpper() == "SYDREPRT.DBF".ToUpper())
                        if (!parentobjectname.ToUpper().Contains("report".ToUpper()))
                            parentobjectname = parentobjectname + "Report";
                    var x = CriteriaOperator.Parse("[ObjectName]='Aria4XP-VFP9-" + parentobjectname.Trim() + "' and [ObjectType]= '" + Entitytype.Oid + "'");


                    Parentobject = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectName]='Aria4XP-VFP9-" + parentobjectname.Trim() + "' and [ObjectType]= '" + Entitytype.Oid + "'"));

                    //select from aria object in sql db all object that end with the originalnamecolumn (in param) and objecttype is an entity 
                    // AriaObject Parentobject = sess.FindObject<AriaObject>(CriteriaOperator.Parse("EndsWith(ObjectName,'" + parentobjectname + "') and [ObjectType]= '" + Entitytype.Oid + "'"));
                    //GroupOperator.Combine(GroupOperatorType.And, CriteriaOperator.Parse("[ObjectType].Name == 'Entity'",new FunctionOperator(FunctionOperatorType.Contains, new OperandProperty("ObjectName"),parentobjectname))));// && [ObjectName] like '%" + item[originalnamecolumn].ToString().Trim() +"'"));

                }

                if (existing != null)
                {
                    #region Update existing object with the input param 
                    if (Parentobject != null)
                    {
                        existing.ParentObjectID = Parentobject;
                    }

                    existing.ObjectType = otype;
                    existing.ObjectName = name;
                    existing.Application = App;
                    existing.ConversionDate = DateTime.Now;
                    //existing.Save();
                    newobject = existing;
                    #endregion
                }
                else
                {
                    #region Create new object with the input param 
                    if (Parentobject != null)
                    {
                        newobject.ParentObjectID = Parentobject;
                    }
                    newobject.ObjectType = otype;
                    newobject.ObjectName = name;
                    newobject.Application = App;
                    newobject.ConversionDate = DateTime.Now;
                    newobject.Save();
                    #endregion
                }

                //CUPSBILL
                if (newobject != null)
                {
                    var objSettingWithSettingType = newobject.AriaObjectSettings.Where(x => x.SettingType != null);
                    int currentcolumn = 0;
                    foreach (DataColumn cl in Entities.Columns)
                    {
                        currentcolumn++;
                        if (objSettingWithSettingType.FirstOrDefault() != null)
                        {
                            AriaObjectSetting setting = objSettingWithSettingType.Where(x => x.SettingType.ActualColumnName != null && x.SettingType.ActualColumnName.ToUpper().Trim() == cl.ColumnName.ToUpper().Trim()).FirstOrDefault();
                            //mm
                            if (setting == null)
                            {
                                setting = new AriaObjectSetting(sess);
                                SettingType NewSettingtype = sess.FindObject<SettingType>(CriteriaOperator.Parse("UPPER([ActualColumnName]) = '" + cl.ColumnName.ToUpper().Trim() + "'"));
                                setting.SettingType = NewSettingtype;
                                //Doaa
                                setting.AriaObjectRevision = newobject.AriaObjectRevisions.FirstOrDefault();
                                //Doaa

                                newobject.AriaObjectSettings.Add(setting);
                                newobject.Save();

                            }

                            // mm

                            if (setting != null ? (setting.SettingType != null ? (setting.ActualColumnName == null ? setting.SettingType.ActualColumnName != null : setting.ActualColumnName != null) : false) : false)
                            {
                                //if the result contain data check if the column name start with M ???
                                if (setting.ActualColumnName != null ? setting.ActualColumnName.ToUpper().StartsWith("M") : setting.SettingType.ActualColumnName.ToUpper().StartsWith("M"))
                                {
                                    try
                                    {
                                        memototextClass Memototext = new memototextClass();
                                        ArrayToMemo.Main x = new ArrayToMemo.Main();
                                        string arrname;
                                        //DataTable memodt = new DataTable();
                                        //string newmemoQuery = "SELECT " + cl.ColumnName.ToString() + " FROM " + tablenamewithext.Split('.')[0] + " WHERE RECNO() = " + row.ToString();
                                        //OleDbCommand newQuery = new OleDbCommand(newmemoQuery, yourConnectionHandler);
                                        //OleDbDataAdapter ddd = new OleDbDataAdapter(newQuery);
                                        //ddd.Fill(memodt);
                                        //var number = memodt.Rows[0][0];
                                        //System.IO.File.WriteAllText(memofieldspath + "" + (row + cl.ColumnName.ToString()).ToUpper() + ".TXT", number.ToString());


                                        //check if setting datatype is memo
                                        if (setting.SettingType.DataType == SettingType.EnumDataType.Memo)
                                        {
                                            bool file_is_exist = System.IO.File.Exists("" + memofieldspath + "" + (row + cl.ColumnName.ToString()).ToUpper() + ".TXT");
                                            if (!file_is_exist)
                                                Memototext.memototextfile(tablenamewithext.Split('.')[0].ToString(), row, cl.ColumnName.ToString(), SysfilePath, memofieldspath);
                                            string memofield = System.IO.File.ReadAllText("" + memofieldspath + "" + (row + cl.ColumnName.ToString()).ToUpper() + ".TXT");
                                            ////BELAL ADD THIS CODE
                                            ////CONVERT THE MEMOFEILD TO ASCII TO SAVE IT INTO DB SQL
                                            ////16/04/2019
                                            setting.Value = convertAsciiTextToHex(memofield);
                                            if(setting.Value == null)
                                            {

                                            }
                                            //mm
                                            setting.DataType = setting.SettingType.DataType.ToString().Trim();
                                            setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //Doaa
                                            setting.AriaObjectRevision = newobject.AriaObjectRevisions.FirstOrDefault();
                                            //Doaa
                                            //mm
                                            setting.Save();
                                            continue;
                                            ////BELAL ADD THIS CODE
                                            ////BELAL COMMENT THIS CODE 
                                            ////CONVERT THE MEMOFEILD TO ASCII TO SAVE IT INTO DB SQL
                                            ////16/04/2019
                                            #region OLD CODE
                                            //if (memofield.Length < 10&& memofield.Length>0)
                                            //{
                                            //    setting.Value = convertAsciiTextToHex(memofield);
                                            //    //mm
                                            //    setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //    //mm
                                            //    setting.Save();
                                            //    continue;
                                            //}
                                            //else
                                            //{
                                            //    setting.Value = convertAsciiTextToHex(memofield);
                                            //    //mm
                                            //    setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //    //mm
                                            //    setting.Save();
                                            //    continue;
                                            //}
                                            ////BELAL COMMENT THIS CODE 
                                            ////CONVERT THE MEMOFEILD TO ASCII TO SAVE IT INTO DB SQL
                                            ////16/04/2019
                                            //Array listofobj = null;
                                            //if (setting.ActualColumnName != null ? setting.ActualColumnName.ToUpper() == "MREPAVFLD" : setting.SettingType.ActualColumnName.ToUpper() == "MREPAVFLD")
                                            //{
                                            //    Array[] listofobj1 = new Array[2];
                                            //    //MemoTotowarrayArray ??
                                            //    listofobj1 = MemoTotowarrayArray(memofield, out arrname);
                                            //    if (listofobj1[0] != null)
                                            //    {
                                            //        AriaDevExpress.Module.BusinessObjects.SysFiles.Fields[] Fields = new AriaDevExpress.Module.BusinessObjects.SysFiles.Fields[listofobj1[0].GetLength(0)];
                                            //        for (int i = 0; i < listofobj1[0].GetLength(0); i++)
                                            //        {
                                            //            AriaDevExpress.Module.BusinessObjects.SysFiles.Fields Field = new AriaDevExpress.Module.BusinessObjects.SysFiles.Fields();
                                            //            Field.Expression = listofobj1[1].GetValue(i, 0).ToString();
                                            //            Field.DisplayName = listofobj1[0].GetValue(i, 0).ToString();
                                            //            Fields[i] = Field;
                                            //        }
                                            //        if (Fields.Length > 0)
                                            //        {
                                            //            FieldsConverter conv = new FieldsConverter();
                                            //            setting.Value = conv.ConvertToStorageType(Fields.ToList<Fields>()).ToString();
                                            //            //mm
                                            //            setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //            //mm
                                            //            setting.Save();
                                            //        }
                                            //    }

                                            //}
                                            //else
                                            //{
                                            //    //var listofobjx = MemoTotowarrayArray(memofield, out arrname);
                                            //    listofobj = x.MemoToArray(memofield, out arrname);
                                            //    if (listofobj.GetLength(1) < 6)
                                            //    {
                                            //        continue;
                                            //    }
                                            //    if (setting.SettingType.ActualColumnName.ToUpper().Contains("LT"))
                                            //    {
                                            //        AriaDevExpress.Module.BusinessObjects.SysFiles.Filter[] filters = new AriaDevExpress.Module.BusinessObjects.SysFiles.Filter[listofobj.GetLength(0)];
                                            //        for (int i = 0; i < listofobj.GetLength(0); i++)
                                            //        {
                                            //            AriaDevExpress.Module.BusinessObjects.SysFiles.Filter filter = new AriaDevExpress.Module.BusinessObjects.SysFiles.Filter();
                                            //            var FilterValuData = listofobj.GetValue(i, 0).ToString();
                                            //            filter.Name = FilterValuData;
                                            //            if (listofobj.GetValue(i, 1) != null && !string.IsNullOrEmpty(listofobj.GetValue(i, 1).ToString()) && !string.IsNullOrWhiteSpace(listofobj.GetValue(i, 1).ToString()))
                                            //            {
                                            //                FilterValuData = listofobj.GetValue(i, 1).ToString();
                                            //                filter.Type = (FilterVariableType)Enum.Parse(typeof(FilterVariableType), FilterValuData);
                                            //            }
                                            //            if (listofobj.GetValue(i, 2) != null && !string.IsNullOrEmpty(listofobj.GetValue(i, 2).ToString()) && !string.IsNullOrWhiteSpace(listofobj.GetValue(i, 2).ToString()))
                                            //            {
                                            //                FilterValuData = listofobj.GetValue(i, 2).ToString();
                                            //                filter.DataType = (SystemDataType)Enum.Parse(typeof(SystemDataType), FilterValuData);
                                            //            }
                                            //            if (listofobj.GetValue(i, 3) != null && !string.IsNullOrEmpty(listofobj.GetValue(i, 3).ToString()) && !string.IsNullOrWhiteSpace(listofobj.GetValue(i, 3).ToString()))
                                            //            {
                                            //                try
                                            //                {
                                            //                    filter.Not = bool.Parse(listofobj.GetValue(i, 3).ToString());
                                            //                }
                                            //                catch
                                            //                {
                                            //                    if (listofobj.GetValue(i, 3).ToString().ToUpper().Contains("T"))
                                            //                    {
                                            //                        filter.Not = true;
                                            //                    }
                                            //                }
                                            //            }
                                            //            if (listofobj.GetValue(i, 4) != null && !string.IsNullOrEmpty(listofobj.GetValue(i, 4).ToString()) && !string.IsNullOrWhiteSpace(listofobj.GetValue(i, 4).ToString()))
                                            //            {
                                            //                FilterValuData = Regex.Replace(listofobj.GetValue(i, 4).ToString(), @"\s+", "");
                                            //                filter.Operator = (FilterOperator)Enum.Parse(typeof(FilterOperator), FilterValuData);
                                            //            }
                                            //            if (listofobj.GetValue(i, 5) != null && !string.IsNullOrWhiteSpace(listofobj.GetValue(i, 5).ToString()))
                                            //            {
                                            //                filter.Value = listofobj.GetValue(i, 5).ToString();
                                            //            }
                                            //            if (listofobj.GetValue(i, 6) != null && !string.IsNullOrEmpty(listofobj.GetValue(i, 6).ToString()) && !string.IsNullOrWhiteSpace(listofobj.GetValue(i, 6).ToString()))
                                            //            {
                                            //                filter.ValueType = (FilterValueType)Enum.Parse(typeof(FilterValueType), listofobj.GetValue(i, 6).ToString());
                                            //            }
                                            //            if (setting.SettingType.ActualColumnName.ToUpper().Contains("HD"))
                                            //                filter.FilterType = AriaDevExpress.Module.BusinessObjects.SysFiles.FilterType.HeddienFilter;
                                            //            if (setting.SettingType.ActualColumnName.ToUpper().Contains("VR"))
                                            //                filter.FilterType = AriaDevExpress.Module.BusinessObjects.SysFiles.FilterType.VariableFilter;
                                            //            if (setting.SettingType.ActualColumnName.ToUpper().Contains("FX"))
                                            //                filter.FilterType = AriaDevExpress.Module.BusinessObjects.SysFiles.FilterType.FixedFilter;
                                            //            filters[i] = filter;
                                            //        }
                                            //        if (filters.Length > 0)
                                            //        {
                                            //            FilterConverter conv = new FilterConverter();
                                            //            setting.Value = conv.ConvertToStorageType(filters.ToList<Filter>()).ToString();
                                            //            //mm
                                            //            setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //            //mm
                                            //            setting.Save();
                                            //        }

                                            //    }
                                            //    else if (setting.SettingType.ActualColumnName.ToUpper().Contains("LD"))
                                            //    {
                                            //        AriaDevExpress.Module.BusinessObjects.SysFiles.Fields[] Fields = new AriaDevExpress.Module.BusinessObjects.SysFiles.Fields[listofobj.GetLength(0)];
                                            //        for (int i = 0; i < listofobj.GetLength(0); i++)
                                            //        {
                                            //            AriaDevExpress.Module.BusinessObjects.SysFiles.Fields Field = new AriaDevExpress.Module.BusinessObjects.SysFiles.Fields();
                                            //            Field.Expression = listofobj.GetValue(i, 0).ToString();
                                            //            Field.DisplayName = listofobj.GetValue(i, 1).ToString();

                                            //            Fields[i] = Field;
                                            //        }
                                            //        if (Fields.Length > 0)
                                            //        {
                                            //            FieldsConverter conv = new FieldsConverter();
                                            //            setting.Value = conv.ConvertToStorageType(Fields.ToList<Fields>()).ToString();
                                            //            //mm
                                            //            setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //            //mm
                                            //            setting.Save();
                                            //        }
                                            //    }
                                            //    else if (setting.SettingType.ActualColumnName.ToUpper().Contains("IL"))
                                            //    {
                                            //        AriaDevExpress.Module.BusinessObjects.SysFiles.Files[] Files = new AriaDevExpress.Module.BusinessObjects.SysFiles.Files[listofobj.GetLength(0)];
                                            //        for (int i = 0; i < listofobj.GetLength(0); i++)
                                            //        {
                                            //            AriaDevExpress.Module.BusinessObjects.SysFiles.Files File = new AriaDevExpress.Module.BusinessObjects.SysFiles.Files();
                                            //            File.Name = listofobj.GetValue(i, 0).ToString();
                                            //            File.Alias = listofobj.GetValue(i, 1).ToString();
                                            //            File.Index = listofobj.GetValue(i, 2).ToString();
                                            //            Files[i] = File;
                                            //        }
                                            //        if (Files.Length > 0)
                                            //        {
                                            //            FilesConverter conv = new FilesConverter();
                                            //            setting.Value = conv.ConvertToStorageType(Files.ToList<Files>()).ToString();
                                            //            //mm
                                            //            setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //            //mm
                                            //            setting.Save();
                                            //        }
                                            //    }
                                            //}
                                            //setting.Value = listofobj.ToString(); 
                                            #endregion
                                        }
                                        else
                                        {

                                            bool file_is_exist = System.IO.File.Exists("" + memofieldspath + "" + (row + cl.ColumnName.ToString()).ToUpper() + ".TXT");
                                            if (!file_is_exist)
                                                Memototext.memototextfile(tablenamewithext.Split('.')[0].ToString(), row, cl.ColumnName.ToString(), SysfilePath, memofieldspath);
                                            string memofield = System.IO.File.ReadAllText("" + memofieldspath + "" + (row + cl.ColumnName.ToString()).ToUpper() + ".TXT");
                                            setting.Value = memofield;
                                            //mm
                                            if (setting.Value == null)
                                            {

                                            }
                                            setting.DataType = setting.SettingType.DataType.ToString().Trim();
                                            setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                            //Doaa
                                            setting.AriaObjectRevision = newobject.AriaObjectRevisions.FirstOrDefault();
                                            //Doaa
                                            //mm
                                            setting.Save();
                                        }
                                    }
                                    catch (Exception Exc)
                                    {
                                        xlWorkSheet.Cells[seq, 1] = tablenamewithext;
                                        xlWorkSheet.Cells[seq, 2] = row;
                                        xlWorkSheet.Cells[seq, 3] = cl.ColumnName.ToString();
                                        xlWorkSheet.Cells[seq, 4] = Exc.Message;
                                        seq++;
                                    }
                                }
                                else
                                {
                                    setting.Value = item[cl].ToString();
                                    //mm
                                    if (setting.Value == null)
                                    {

                                    }
                                    setting.DataType = setting.SettingType.DataType.ToString().Trim ();
                                    setting.ActualColumnName = setting.SettingType.ActualColumnName.ToUpper().Trim();
                                    //Doaa
                                    setting.AriaObjectRevision = newobject.AriaObjectRevisions.FirstOrDefault();
                                    //Doaa
                                    //mm
                                    setting.Save();
                                }

                            }

                        }

                    }
                }
                if (existing != null)
                    existing.Save();

            }

            xlWorkBook.SaveAs("D:\\uploadsysfilesexceptions" + tablenamewithext.Replace(".", "") + DateTime.Now.ToString("MMddyyyy") + ".xls", Excel.XlFileFormat.xlWorkbookNormal, misValue, misValue, misValue, misValue, Excel.XlSaveAsAccessMode.xlExclusive, misValue, misValue, misValue, misValue, misValue);
            xlWorkBook.Close(true, misValue, misValue);
            xlApp.Quit();

            releaseObject(xlWorkSheet);
            releaseObject(xlWorkBook);
            releaseObject(xlApp);

        }
        //ATA 12/25/2017 
        public static void LinksysfilestoEntities(string sysfilename)
        {
            DataTable Entities = new DataTable();
            OleDbConnection yourConnectionHandler = new OleDbConnection(
               @"Provider=VFPOLEDB.1;Data Source=D:\Aria4xp\Aria4xp (R13)\\sysfiles\\");

            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from  " + sysfilename + "";  // dbf table name

            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(Entities);
            Session sess = new Session();
            sess.ConnectionString = connectionstr;
            sess.Connect();



            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes("D:\\"));
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 3; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            { }
        }


        public static void LinkFILEFIELD()
        {
            DataTable Entities = new DataTable();
            OleDbConnection yourConnectionHandler = new OleDbConnection(
               @"Provider=VFPOLEDB.1;Data Source=D:\Aria4xp\Aria4xp (R13)\sysfiles\\");

            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from SYDFLFLD.DBF";  // dbf table name
            int count = 0;
            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(Entities);
            Session sess = new Session();
            sess.ConnectionString = connectionstr;
            sess.Connect();

            ObjectType filetype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Ariafile'"));
            ObjectType fieldtype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'AriaField'"));

            foreach (DataRow Record in Entities.Rows)
            {
                string filename = Record["Cfile_nam"].ToString().Trim();
                string fieldname = Record["Cfld_name"].ToString().Trim();
                int fieldpos = int.Parse(Record["Nfld_pos"].ToString().Trim());
                string Upgradelevel = Record["cupgrdlvl"].ToString().Trim();


                try
                {

                    AriaObject file = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectName] = '" + filename + "' and [ObjectType] = '" + filetype.Oid + "'"));
                    AriaObject Field = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectName] = '" + fieldname.Trim() + "' and [ObjectType] = '" + fieldtype.Oid + "'"));
                    if (file != null && Field != null)
                    {
                        AriaObjectProperty property = new AriaObjectProperty(sess);
                        property.RelatedAriaObject = Field;
                        property.PropertyType = sess.FindObject<PropertyType>(CriteriaOperator.Parse("[Name] = '" + fieldtype.Name + "'"));
                        //only commented it to overcome the error//property.FieldPosition = fieldpos;
                        //only commented it to overcome the error//property.UpgradeLevel = Upgradelevel;
                        file.AriaObjectProperties.Add(property);
                        file.Save();
                    }
                }
                catch (Exception)
                {
                    count++;

                    using (System.IO.StreamWriter file =
                     new System.IO.StreamWriter(@"D:\Test.txt", true))
                    {
                        file.WriteLine(filename + "   " + count);
                    }

                }


            }



        }

        public static Array[] MemoTotowarrayArray(string Memo, out string ArrayName)
        {
            //  char[] x = Memo.ToArray<char>();
            //  char xx = x[34];
            //  char yy = x[36];
            ArrayName = "";
            int num = 2;
            while (num < 13)
            {
                //Memo[num] is null 
                if (Memo[num] == '\0')
                {
                    break;
                }
                ArrayName = ArrayName + Memo[num];
                num++;
            }

            // position 34
            int num2 = Memo[0x22];
            // position 36
            int num3 = Memo[0x24];
            Array[] array = new Array[2];
            if (num3 == 0)
            {
                array[0] = new object[num2];
            }
            else
            {
                array[0] = new object[num2, num3];
            }
            bool flag = true;
            char ch = '\0';
            object obj2 = null;
            int num4 = 0x26;
            int index = 0;
            int num6 = 0;
            while (flag)
            {
                ch = Memo[num4 + 11];
                obj2 = null;
                if (ch == 'L')
                {
                    obj2 = Memo[num4 + 0x20] != '\0';
                    num4 += 0x20;
                }
                else
                {
                    obj2 = "";
                    for (num = num4 + 0x20; num < Memo.Length; num++)
                    {
                        if (Memo[num] != '\0')
                        {
                            obj2 = obj2.ToString() + Memo[num];
                        }
                        else
                        {
                            num4 = num;
                            break;
                        }
                    }
                }
                if (num3 == 0)
                {
                    array[0].SetValue(obj2, index);
                    index++;
                }
                else
                {
                    array[0].SetValue(obj2, index, num6);
                    if ((num6 + 1) == num3)
                    {
                        num6 = 0;
                        index++;
                    }
                    else
                    {
                        num6++;
                    }
                }
                if ((Memo[num4 + 1] == '\x001a') || ((index == (num2)) && (num6 == (num3 - 1))))
                {
                    array[1] = getsecondMemoToArray(Memo.Remove(3, num4 - 1), out ArrayName);
                    flag = false;
                }
                else
                {
                    num4++;
                }
            }
            return array;
        }
        public static Array getsecondMemoToArray(string Memo, out string ArrayName)
        {
            //  char[] x = Memo.ToArray<char>();
            // char xx = x[34];
            // char yy = x[36];
            Array array = null;
            if (Memo.Length < 5)
            {
                ArrayName = "Empty";
                return array;
            }
            ArrayName = "";
            int num = 2;
            while (num < 13)
            {
                if (Memo[num] == '\0')
                {
                    break;
                }
                ArrayName = ArrayName + Memo[num];
                num++;
            }
            // position 34
            int num2 = Memo[0x22];
            // position 36
            int num3 = Memo[0x24];

            if (num3 == 0)
            {
                array = new object[num2];
            }
            else
            {
                array = new object[num2, num3];
            }
            bool flag = true;
            char ch = '\0';
            object obj2 = null;
            int num4 = 0x26;
            int index = 0;
            int num6 = 0;
            while (flag)
            {
                ch = Memo[num4 + 11];
                obj2 = null;
                if (ch == 'L')
                {
                    obj2 = Memo[num4 + 0x20] != '\0';
                    num4 += 0x20;
                }
                else
                {
                    obj2 = "";
                    for (num = num4 + 0x20; num < Memo.Length; num++)
                    {
                        if (Memo[num] != '\0')
                        {
                            obj2 = obj2.ToString() + Memo[num];
                        }
                        else
                        {
                            num4 = num;
                            break;
                        }
                    }
                }
                if (num3 == 0)
                {
                    array.SetValue(obj2, index);
                    index++;
                }
                else
                {
                    array.SetValue(obj2, index, num6);
                    if ((num6 + 1) == num3)
                    {
                        num6 = 0;
                        index++;
                    }
                    else
                    {
                        num6++;
                    }
                }
                if ((Memo[num4 + 1] == '\x001a') || ((index == (num2)) && (num6 == (num3 - 1))))
                {
                    flag = false;
                }
                else
                {
                    num4++;
                }
            }
            return array;
        }
        public void sysfileextract()
        {
            #region sysfilesextract
            System.Data.SqlClient.SqlCommand cmd = new System.Data.SqlClient.SqlConnection(connectionstr).CreateCommand();

            string sqlSelect = "Select * from {0} {1}";

            string where = "Where [crep_id] = 'sorddet'";

            DataSet data = new DataSet();
            data.DataSetName = "SysFilesXML";

            cmd.CommandText = string.Format(sqlSelect, "sydreprt", where);
            System.Data.SqlClient.SqlDataAdapter da = new System.Data.SqlClient.SqlDataAdapter(cmd);
            da.Fill(data);
            // data.Tables[data.Tables.Count - 1].TableName = item.Value.ToString();
            Session ss = new Session();
            ss.ConnectionString = connectionstr;
            ss.Connect();
            AriaDevExpress.Module.BusinessObjects.SysFiles.Report rp = ss.FindObject<AriaDevExpress.Module.BusinessObjects.SysFiles.Report>(CriteriaOperator.Parse("[ID] = 'sorddet'"));

            // ArrayToMemo.Main xx = new ArrayToMemo.Main();
            //string getmemo  = ArrayToMemo(rp.FixedFilter.ToArray(), "laogfxflt");
            using (var zip = new ZipFile())
            {
                foreach (DataTable table in data.Tables)
                {
                    MemoryStream memStream = new MemoryStream();

                    if (rp != null)
                    {
                        ArrayToMemo.Main xxy = new ArrayToMemo.Main();
                        //  getmemo = xxy.ArrayToMemo(rp.FixedFilter.ToArray(), "laogfxflt");
                        object[,] arrayofobj = new object[6, 7];
                        //for (int i = 0;i< rp.FixedFilter.Count;i++)
                        //{
                        //    arrayofobj[i,0] = rp.FixedFilter[i].Name;
                        //    arrayofobj[i,1] = rp.FixedFilter[i].Type;
                        //    arrayofobj[i,2] = rp.FixedFilter[i].DataType;
                        //    arrayofobj[i,3] = rp.FixedFilter[i].Not;
                        //    arrayofobj[i,4] = rp.FixedFilter[i].Operator;
                        //    arrayofobj[i,5] = rp.FixedFilter[i].Value;
                        //    arrayofobj[i,6] = rp.FixedFilter[i].ValueType;
                        //}
                        arrayofobj[0, 0] = "Ordline.Style";
                        arrayofobj[0, 1] = "F";
                        arrayofobj[0, 2] = "C";
                        arrayofobj[0, 3] = false;
                        arrayofobj[0, 4] = "In List ";
                        arrayofobj[0, 5] = " ";
                        arrayofobj[0, 6] = "V";
                        arrayofobj[1, 0] = "Ordline.Season";
                        arrayofobj[1, 1] = "F";
                        arrayofobj[1, 2] = "C";
                        arrayofobj[1, 3] = false;
                        arrayofobj[1, 4] = "In List ";
                        arrayofobj[1, 5] = " ";
                        arrayofobj[1, 6] = "V";
                        arrayofobj[2, 0] = "Style.Pattern";
                        arrayofobj[2, 1] = "F";
                        arrayofobj[2, 2] = "C";
                        arrayofobj[2, 3] = false;
                        arrayofobj[2, 4] = "In List ";
                        arrayofobj[2, 5] = " ";
                        arrayofobj[2, 6] = "V";
                        arrayofobj[3, 0] = "Ordhdr.Entered";
                        arrayofobj[3, 1] = "F";
                        arrayofobj[3, 2] = "D";
                        arrayofobj[3, 3] = false;
                        arrayofobj[3, 4] = "Between ";
                        arrayofobj[3, 5] = " ";
                        arrayofobj[3, 6] = "V";
                        arrayofobj[4, 0] = "Ordhdr.Start";
                        arrayofobj[4, 1] = "F";
                        arrayofobj[4, 2] = "D";
                        arrayofobj[4, 3] = false;
                        arrayofobj[4, 4] = "Between ";
                        arrayofobj[4, 5] = " ";
                        arrayofobj[4, 6] = "V";
                        arrayofobj[5, 0] = "ordhdr.order";
                        arrayofobj[5, 1] = "F";
                        arrayofobj[5, 2] = "C";
                        arrayofobj[5, 3] = false;
                        arrayofobj[5, 4] = "In List ";
                        arrayofobj[5, 5] = " ";
                        arrayofobj[5, 6] = "V";
                        System.IO.File.WriteAllText("D:\\mmm1.txt", ArrayToMemo(arrayofobj, "laogfxflt"));
                        table.Rows[0]["MrepFxFlt"] = ArrayToMemo(arrayofobj, "laogfxflt");
                        System.IO.File.WriteAllText("D:\\mmm2.txt", table.Rows[0]["MrepFxFlt"].ToString());

                    }
                    //table.WriteXml(memStream);

                    String sqlCommandInsert = "INSERT INTO {0}(";
                    String sqlCommandValue = "";
                    for (int i = 1; i < table.Columns.Count; i++)
                    {
                        if (i >= 12 && i <= 14 || i > 18)
                        {
                            continue;
                        }
                        sqlCommandInsert += table.Columns[i].ColumnName + ",";
                    }
                    sqlCommandInsert += sqlCommandInsert.TrimEnd(',');

                    sqlCommandInsert += ") VALUES";

                    for (int i = 0; i < table.Rows.Count; i++)
                    {
                        sqlCommandValue += "(";
                        for (int y = 1; y < table.Columns.Count; y++)
                        {
                            if (i >= 12 && i <= 14 || i > 18)
                            {
                                continue;
                            }
                            if (i == 18)
                            {
                                sqlCommandValue += "FILETOSTR('D:\\mmm.txt'))";
                                continue;
                            }
                            sqlCommandValue += "'" + table.Rows[i][table.Columns[y].ColumnName] + "',";
                        }
                        sqlCommandValue += ")";
                    }
                    //  var insertCommand = sqlCommandInsert;
                    sqlCommandValue = sqlCommandValue.TrimEnd(',');

                    var command = sqlCommandInsert + sqlCommandValue + ")";
                    addtotable.func(command);
                    memStream.Position = 0;
                    zip.AddEntry(table.TableName + ".xml", memStream);
                }
                zip.Save("D:\\test.zip");
            }
            #endregion
        }

        public void insertariaobjects()
        {
            ////////////////////////////////////////////////////
            //    newobject.ObjectName = name;
            //    newobject.ObjectType = sess.FindObject<ObjectType>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[i][4].ToString() + "'"));
            //    if (dataTable.Rows[i][1].ToString() != "")
            //    {
            //        basetestcase = sess.FindObject<AriaObject>(CriteriaOperator.Parse("ObjectID = '" + dataTable.Rows[i][1].ToString() + "'"));
            //        if (basetestcase != null)
            //        {
            //            newobject.ParentObjectID = basetestcase;
            //        }
            //    }
            //    else if (sess.FindObject<AriaObject>(CriteriaOperator.Parse("ObjectID = '" + dataTable.Rows[i][1].ToString() + "'")) != null)
            //    {
            //        // baseparent = sess.FindObject<AriaObject>(CriteriaOperator.Parse("ObjectID = '" + dataTable.Rows[i][1].ToString() + "'"));
            //        if (basetestcase != null)
            //        {
            //            newobject.ParentObjectID = basetestcase;
            //        }
            //    }

            //    newobject.Save();
            //    if (newobject.AriaObjectSettings.Count > 0)
            //    {
            //        AriaObjectSetting Storagefile = newobject.AriaObjectSettings.Where(x => x.SettingType.Name == "Storage File Name").FirstOrDefault();
            //        if (Storagefile != null)
            //        {
            //            Storagefile.Value = dataTable.Rows[i][2].ToString();
            //            Storagefile.Save();
            //        }
            //        AriaObjectSetting subfolder = newobject.AriaObjectSettings.Where(x => x.SettingType.Name == "Sub Folder").FirstOrDefault();
            //        if (subfolder != null)
            //        {
            //            subfolder.Value = dataTable.Rows[i][3].ToString();
            //            subfolder.Save();
            //        }
            //    }
            //    for (int childs = (i + 1); childs < (i + 3); childs++)
            //    {
            //        if (childs != dataTable.Rows.Count)
            //        {
            //            AriaObject newobject1 = new AriaObject(sess);
            //            string name1 = dataTable.Rows[childs][0].ToString();
            //            newobject1.ObjectName = name1;
            //            newobject1.ObjectType = sess.FindObject<ObjectType>(CriteriaOperator.Parse("Name = '" + dataTable.Rows[childs][4].ToString() + "'"));
            //            newobject1.ParentObjectID = newobject;
            //            newobject1.Save();
            //            if (newobject1.AriaObjectSettings.Count > 0)
            //            {
            //                AriaObjectSetting Storagefile = newobject1.AriaObjectSettings.Where(x => x.SettingType.Name == "Storage File Name").FirstOrDefault();
            //                if (Storagefile != null)
            //                {
            //                    Storagefile.Value = dataTable.Rows[childs][2].ToString();
            //                    Storagefile.Save();
            //                }
            //                AriaObjectSetting subfolder = newobject1.AriaObjectSettings.Where(x => x.SettingType.Name == "Sub Folder").FirstOrDefault();
            //                if (subfolder != null)
            //                {
            //                    subfolder.Value = dataTable.Rows[childs][3].ToString();
            //                    subfolder.Save();
            //                }
            //            }
            //        }
            //    }
            //    i = i + 2;

        }
        public static IList<string> Getfiles(string path, IList<string> filePaths)
        {
            string[] files = Directory.GetFiles(path);
            string[] folders = Directory.GetDirectories(path);
            foreach (string file in files)
            {
                filePaths.Add(file);
            }
            if (folders.Count() > 0)
            {
                for (int i = 0; i < 15; i++)
                {
                    foreach (string item in Getfiles(Path.GetFullPath(folders[i].ToString()), filePaths).ToArray<string>())
                    {
                        filePaths.Add(item);
                    }
                }


            }
            return filePaths;

        }
        public static void extractprojecttype()
        {
            IList<string> listoffiles = new List<string>();
            IList<string> filePaths = Getfiles(@"D:\ARIA4XP\Aria4xp (R13)\", listoffiles);
            Excel.Application xlApp = new Microsoft.Office.Interop.Excel.Application();
            Excel.Workbook xlWorkBook;
            Excel.Worksheet xlWorkSheet;
            object misValue = System.Reflection.Missing.Value;

            xlWorkBook = xlApp.Workbooks.Add(misValue);
            xlWorkSheet = (Excel.Worksheet)xlWorkBook.Worksheets.get_Item(1);
            if (xlApp == null)
            {

                return;
            }
            for (int i = 0; i < filePaths.Count; i++)
            {
                int y = 1;

                if (i > 0)
                {
                    y = i + 1;
                }


                xlWorkSheet.Cells[y, 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString();
                xlWorkSheet.Cells[y, 2] = Path.GetPathRoot(filePaths[i].ToString());
                xlWorkSheet.Cells[y, 3] = Path.GetFileName(filePaths[i]).ToString();
                xlWorkSheet.Cells[y, 4] = Path.GetExtension(filePaths[i].ToString());

                //xlWorkSheet.Cells[(y + 1), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Class";
                //xlWorkSheet.Cells[(y + 1), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                //xlWorkSheet.Cells[(y + 1), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.cs";

                //xlWorkSheet.Cells[(y + 2), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Designer";
                //xlWorkSheet.Cells[(y + 2), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                //xlWorkSheet.Cells[(y + 2), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.designer.cs";


                i = i++;
            }

            xlWorkBook.SaveAs("D:\\Files.xls", Excel.XlFileFormat.xlWorkbookNormal, misValue, misValue, misValue, misValue, Excel.XlSaveAsAccessMode.xlExclusive, misValue, misValue, misValue, misValue, misValue);
            xlWorkBook.Close(true, misValue, misValue);
            xlApp.Quit();

            releaseObject(xlWorkSheet);
            releaseObject(xlWorkBook);
            releaseObject(xlApp);
        }
        public static void releaseObject(object obj)
        {
            try
            {
                System.Runtime.InteropServices.Marshal.ReleaseComObject(obj);
                obj = null;
            }
            catch (Exception ex)
            {
                obj = null;
            }
            finally
            {
                GC.Collect();
            }
        }

        public static void getAria4entities()
        {
            DataTable SydReportSet = new DataTable();
            DataTable ListofphysicalFiles = new DataTable();
            DataTable SydObjectSet = new DataTable();

            IList<string> Related = new List<string>();
            string mrelprograms;
            string programpart;
            OleDbConnection yourConnectionHandler = new OleDbConnection(
                @"Provider=VFPOLEDB.1;Data Source=D:\ARIA4XP\Aria4xp (R13)\\sysfiles\\");

            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from  SYDREPRT.dbf";  // dbf table name

            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);

            DA.Fill(SydReportSet);

            mySQL = "select * from  SYDOBJCT.dbf";

            OleDbConnection yourConnectionHandler1 = new OleDbConnection(
                @"Provider=VFPOLEDB.1;Data Source=D:\\");
            //}
            MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(SydObjectSet);

            string mySQL1 = "select * from  Files.dbf";  // dbf table name
            OleDbCommand MyQuery1 = new OleDbCommand(mySQL1, yourConnectionHandler1);
            OleDbDataAdapter DA1 = new OleDbDataAdapter(MyQuery1);

            DA1.Fill(ListofphysicalFiles);
            ListofphysicalFiles.CaseSensitive = false;
            string path = "";
            int rownumber = 1;
            Excel.Application xlApp = new Microsoft.Office.Interop.Excel.Application();
            Excel.Workbook xlWorkBook;
            Excel.Worksheet xlWorkSheet;
            object misValue = System.Reflection.Missing.Value;
            xlWorkBook = xlApp.Workbooks.Add(misValue);
            xlWorkSheet = (Excel.Worksheet)xlWorkBook.Worksheets.get_Item(1);
            #region syreport
            for (int r = 0; r < SydReportSet.Rows.Count; r++)
            {

                //  foreach (DataColumn item in YourResultSet.Columns)
                // {
                //if (memofields.Contains(item.ColumnName.ToUpper()))
                //{
                //    string text = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //    YourResultSet.Rows[r][item.ColumnName] = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //}
                mrelprograms = SydReportSet.Rows[r]["mrelprogs"].ToString();
                string[] x = mrelprograms.Split(',');
                for (int i = 0; i < x.Length; i++)
                {
                    //if (mrelprograms.IndexOf(",") > 0)
                    //{

                    programpart = x[i];
                    //mrelprograms.Substring(0, mrelprograms.IndexOf(",") - 0);
                    programpart = programpart.TrimEnd(new char[] { '\r', '\n' });
                    if (Path.GetExtension(programpart).Trim().ToUpper() == ".FXP")
                    {
                        programpart = programpart.Replace(".FXP", ".PRG");
                    }
                    Related.Add(programpart.ToUpper().Trim());
                    //   mrelprograms = //mrelprograms.Remove(0, mrelprograms.IndexOf(",") + 1);
                    //}
                    //else
                    //{
                    //    programpart = mrelprograms;
                    //}

                    DataRow Selectedrow = ListofphysicalFiles.Select("Name1 = '" + programpart.Trim().ToUpper() + "'").FirstOrDefault();
                    // name = Selectedrow["Name1"].ToString();
                    if (Selectedrow != null)
                    {
                        path = Selectedrow["Path1"].ToString();
                    }
                    else
                    {
                        path = " ";
                    }

                    if (xlApp == null)
                    {

                        return;
                    }
                    //xlWorkSheet.Cells[rownumber, 1] = YourResultSet.Rows[r]["Crep_name"].ToString();
                    //xlWorkSheet.Cells[rownumber, 2] = programpart;
                    //xlWorkSheet.Cells[rownumber, 3] = path;
                    //    rownumber++;
                    //xlWorkSheet.Cells[y, 3] = Path.GetFileName(filePaths[i]).ToString();
                    //xlWorkSheet.Cells[y, 4] = Path.GetExtension(filePaths[i].ToString());

                    //xlWorkSheet.Cells[(y + 1), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Class";
                    //xlWorkSheet.Cells[(y + 1), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 1), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.cs";

                    //xlWorkSheet.Cells[(y + 2), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Designer";
                    //xlWorkSheet.Cells[(y + 2), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 2), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.designer.cs";


                }


                // }
            }
            #endregion
            #region sydobject
            for (int r = 0; r < SydObjectSet.Rows.Count; r++)
            {

                //  foreach (DataColumn item in YourResultSet.Columns)
                // {
                //if (memofields.Contains(item.ColumnName.ToUpper()))
                //{
                //    string text = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //    YourResultSet.Rows[r][item.ColumnName] = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //}
                mrelprograms = SydObjectSet.Rows[r]["mrelprogs"].ToString();
                string[] x = mrelprograms.Split(',');
                for (int i = 0; i < x.Length; i++)
                {
                    //if (mrelprograms.IndexOf(",") > 0)
                    //{

                    programpart = x[i];
                    //mrelprograms.Substring(0, mrelprograms.IndexOf(",") - 0);
                    programpart = programpart.TrimEnd(new char[] { '\r', '\n' });
                    if (Path.GetExtension(programpart).Trim().ToUpper() == ".FXP")
                    {
                        programpart = programpart.Replace(".FXP", ".PRG");
                    }
                    Related.Add(programpart.ToUpper().Trim());
                    //   mrelprograms = //mrelprograms.Remove(0, mrelprograms.IndexOf(",") + 1);
                    //}
                    //else
                    //{
                    //    programpart = mrelprograms;
                    //}

                    //DataRow Selectedrow = objects.Select("Name1 = '" + programpart.Trim().ToUpper() + "'").FirstOrDefault();
                    //// name = Selectedrow["Name1"].ToString();
                    //if (Selectedrow != null)
                    //{
                    //    path = Selectedrow["Path1"].ToString();
                    //}
                    //else
                    //{
                    //    path = " ";
                    //}

                    //if (xlApp == null)
                    //{

                    //    return;
                    //}
                    //xlWorkSheet.Cells[rownumber, 1] = YourResultSet.Rows[r]["Crep_name"].ToString();
                    //xlWorkSheet.Cells[rownumber, 2] = programpart;
                    //xlWorkSheet.Cells[rownumber, 3] = path;
                    //    rownumber++;
                    //xlWorkSheet.Cells[y, 3] = Path.GetFileName(filePaths[i]).ToString();
                    //xlWorkSheet.Cells[y, 4] = Path.GetExtension(filePaths[i].ToString());

                    //xlWorkSheet.Cells[(y + 1), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Class";
                    //xlWorkSheet.Cells[(y + 1), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 1), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.cs";

                    //xlWorkSheet.Cells[(y + 2), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Designer";
                    //xlWorkSheet.Cells[(y + 2), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 2), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.designer.cs";


                }


                // }
            }
            #endregion
            for (int r = 0; r < ListofphysicalFiles.Rows.Count; r++)
            {
                string FIlename = ListofphysicalFiles.Rows[r]["Name1"].ToString();
                path = ListofphysicalFiles.Rows[r]["path1"].ToString();
                string Exten = ListofphysicalFiles.Rows[r]["extention"].ToString();
                if (!Related.Contains(FIlename.ToUpper().Trim()) && !FIlename.StartsWith("="))
                {
                    // xlWorkSheet.Cells[rownumber, 1] = YourResultSet.Rows[r]["Crep_name"].ToString();
                    xlWorkSheet.Cells[rownumber, 1] = FIlename;
                    xlWorkSheet.Cells[rownumber, 2] = path;
                    xlWorkSheet.Cells[rownumber, 3] = Exten;
                    rownumber++;
                }

            }

            rownumber++;
            xlWorkBook.SaveAs("D:\\notrelatedfiles.xls", Excel.XlFileFormat.xlWorkbookNormal, misValue, misValue, misValue, misValue, Excel.XlSaveAsAccessMode.xlExclusive, misValue, misValue, misValue, misValue, misValue);
            xlWorkBook.Close(true, misValue, misValue);
            xlApp.Quit();

            releaseObject(xlWorkSheet);
            releaseObject(xlWorkBook);
            releaseObject(xlApp);
        }

        public static void getaria3ediEntities()
        {
            DataTable SydReportSet = new DataTable();
            DataTable ListofphysicalFiles = new DataTable();
            DataTable SydObjectSet = new DataTable();

            IList<string> Related = new List<string>();
            string mrelprograms;
            string programpart;
            OleDbConnection yourConnectionHandler = new OleDbConnection(
                @"Provider=VFPOLEDB.1;Data Source=D:\ARIA4XP\Aria4xp (R13)\\sysfiles\\");

            //if (yourConnectionHandler.State == ConnectionState.Open)
            //{
            string mySQL = "select * from  SYDREPRT.dbf";  // dbf table name

            OleDbCommand MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            OleDbDataAdapter DA = new OleDbDataAdapter(MyQuery);

            DA.Fill(SydReportSet);

            mySQL = "select * from  SYDOBJCT.dbf";

            OleDbConnection yourConnectionHandler1 = new OleDbConnection(
                @"Provider=VFPOLEDB.1;Data Source=D:\\");
            //}
            MyQuery = new OleDbCommand(mySQL, yourConnectionHandler);
            DA = new OleDbDataAdapter(MyQuery);
            DA.Fill(SydObjectSet);

            string mySQL1 = "select * from  EDIFiles.dbf";  // dbf table name
            OleDbCommand MyQuery1 = new OleDbCommand(mySQL1, yourConnectionHandler1);
            OleDbDataAdapter DA1 = new OleDbDataAdapter(MyQuery1);

            DA1.Fill(ListofphysicalFiles);
            ListofphysicalFiles.CaseSensitive = false;
            string path = "";
            int rownumber = 1;
            int rownumber2 = 1;
            int rownumber3 = 1;
            Excel.Application xlApp = new Microsoft.Office.Interop.Excel.Application();
            Excel.Workbook xlWorkBook;
            Excel.Worksheet xlWorkSheet;
            Excel.Worksheet xlWorkSheet2;
            Excel.Worksheet xlWorkSheet3;
            object misValue = System.Reflection.Missing.Value;
            xlWorkBook = xlApp.Workbooks.Add(misValue);
            xlWorkSheet = (Excel.Worksheet)xlWorkBook.Worksheets.get_Item(1);
            xlWorkSheet2 = (Excel.Worksheet)xlWorkBook.Worksheets.Add();
            xlWorkSheet3 = (Excel.Worksheet)xlWorkBook.Worksheets.Add();
            #region syreport
            for (int r = 0; r < SydReportSet.Rows.Count; r++)
            {

                //  foreach (DataColumn item in YourResultSet.Columns)
                // {
                //if (memofields.Contains(item.ColumnName.ToUpper()))
                //{
                //    string text = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //    YourResultSet.Rows[r][item.ColumnName] = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //}
                mrelprograms = SydReportSet.Rows[r]["mrelprogs"].ToString();
                string[] x = mrelprograms.Split(',');
                for (int i = 0; i < x.Length; i++)
                {
                    programpart = x[i];
                    programpart = programpart.TrimEnd(new char[] { '\r', '\n' });
                    if (Path.GetExtension(programpart).Trim().ToUpper() == ".FXP")
                    {
                        programpart = programpart.Replace(".FXP", ".PRG");
                    }
                    Related.Add(programpart.ToUpper().Trim());

                    DataRow Selectedrow = ListofphysicalFiles.Select("Name1 = '" + programpart.Trim().ToUpper() + "'").FirstOrDefault();
                    // name = Selectedrow["Name1"].ToString();
                    if (Selectedrow != null)
                    {
                        path = Selectedrow["Path1"].ToString();
                    }
                    else
                    {
                        path = " ";
                    }

                    if (xlApp == null)
                    {

                        return;
                    }
                    xlWorkSheet.Cells[rownumber, 1] = SydReportSet.Rows[r]["Crep_name"].ToString();
                    xlWorkSheet.Cells[rownumber, 2] = programpart;
                    xlWorkSheet.Cells[rownumber, 3] = path;
                    rownumber++;
                    //xlWorkSheet.Cells[y, 3] = Path.GetFileName(filePaths[i]).ToString();
                    //xlWorkSheet.Cells[y, 4] = Path.GetExtension(filePaths[i].ToString());

                    //xlWorkSheet.Cells[(y + 1), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Class";
                    //xlWorkSheet.Cells[(y + 1), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 1), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.cs";

                    //xlWorkSheet.Cells[(y + 2), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Designer";
                    //xlWorkSheet.Cells[(y + 2), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 2), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.designer.cs";


                }


                // }
            }
            #endregion
            #region sydobject
            for (int r = 0; r < SydObjectSet.Rows.Count; r++)
            {

                //  foreach (DataColumn item in YourResultSet.Columns)
                // {
                //if (memofields.Contains(item.ColumnName.ToUpper()))
                //{
                //    string text = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //    YourResultSet.Rows[r][item.ColumnName] = System.IO.File.ReadAllText("D:\\Temp\\" + (r + 1) + item.ColumnName.ToUpper() + ".txt");
                //}
                mrelprograms = SydObjectSet.Rows[r]["mrelprogs"].ToString();
                string[] x = mrelprograms.Split(',');
                for (int i = 0; i < x.Length; i++)
                {
                    //if (mrelprograms.IndexOf(",") > 0)
                    //{

                    programpart = x[i];
                    //mrelprograms.Substring(0, mrelprograms.IndexOf(",") - 0);
                    programpart = programpart.TrimEnd(new char[] { '\r', '\n' });
                    if (Path.GetExtension(programpart).Trim().ToUpper() == ".FXP")
                    {
                        programpart = programpart.Replace(".FXP", ".PRG");
                    }
                    Related.Add(programpart.ToUpper().Trim());
                    //   mrelprograms = //mrelprograms.Remove(0, mrelprograms.IndexOf(",") + 1);
                    //}
                    //else
                    //{
                    //    programpart = mrelprograms;
                    //}

                    DataRow Selectedrow = ListofphysicalFiles.Select("Name1 = '" + programpart.Trim().ToUpper() + "'").FirstOrDefault();
                    // name = Selectedrow["Name1"].ToString();
                    if (Selectedrow != null)
                    {
                        path = Selectedrow["Path1"].ToString();
                    }
                    else
                    {
                        path = " ";
                    }

                    if (xlApp == null)
                    {

                        return;
                    }
                    xlWorkSheet2.Cells[rownumber2, 1] = SydObjectSet.Rows[r]["Cprglname"].ToString();
                    xlWorkSheet2.Cells[rownumber2, 2] = programpart;
                    xlWorkSheet2.Cells[rownumber2, 3] = path;
                    rownumber2++;
                    //xlWorkSheet.Cells[y, 3] = Path.GetFileName(filePaths[i]).ToString();
                    //xlWorkSheet.Cells[y, 4] = Path.GetExtension(filePaths[i].ToString());

                    //xlWorkSheet.Cells[(y + 1), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Class";
                    //xlWorkSheet.Cells[(y + 1), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 1), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.cs";

                    //xlWorkSheet.Cells[(y + 2), 1] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + " Designer";
                    //xlWorkSheet.Cells[(y + 2), 2] = Path.GetFileNameWithoutExtension(filePaths[i]);
                    //xlWorkSheet.Cells[(y + 2), 3] = Path.GetFileNameWithoutExtension(filePaths[i]).ToString() + ".bo.designer.cs";


                }


                // }
            }
            #endregion
            for (int r = 0; r < ListofphysicalFiles.Rows.Count; r++)
            {
                string FIlename = ListofphysicalFiles.Rows[r]["Name1"].ToString();
                path = ListofphysicalFiles.Rows[r]["path1"].ToString();
                string Exten = ListofphysicalFiles.Rows[r]["extention"].ToString();
                if (!Related.Contains(FIlename.ToUpper().Trim()) && !FIlename.StartsWith("="))
                {
                    // xlWorkSheet.Cells[rownumber, 1] = YourResultSet.Rows[r]["Crep_name"].ToString();
                    xlWorkSheet3.Cells[rownumber3, 1] = FIlename;
                    xlWorkSheet3.Cells[rownumber3, 2] = path;
                    xlWorkSheet3.Cells[rownumber3, 3] = Exten;
                    rownumber3++;
                }

            }

            //rownumber3++;
            xlWorkBook.SaveAs("D:\\Edigetfiles.xls", Excel.XlFileFormat.xlWorkbookNormal, misValue, misValue, misValue, misValue, Excel.XlSaveAsAccessMode.xlExclusive, misValue, misValue, misValue, misValue, misValue);
            xlWorkBook.Close(true, misValue, misValue);
            xlApp.Quit();

            releaseObject(xlWorkSheet);
            releaseObject(xlWorkBook);
            releaseObject(xlApp);
        }
        //public static void Checkprojectalarmtimes(ProjectTemplate Notification_Project)
        //{
        //    Notification_Project.Session.LockingOption = LockingOption.None;
        //    if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)))
        //    {
        //            string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //            Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
        //            FillEmailBody(Emailbody, Notification_Project, false);
        //    }
        //    else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)))
        //    {
        //            string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //            Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready or missing: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)));
        //            FillEmailBody(Emailbody, Notification_Project, false);
        //    }
        //    else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 2)).AddDays(1))
        //    {
        //        string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //        Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some objects not ready or missing yet and the base line was yesterday so checke the NC's that created related to this project <br /> Details of the object(s) not ready or missing: <br /><br />");
        //        FillEmailBody(Emailbody, Notification_Project, true);
        //    }
        //    else if (DateTime.Today >= (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3).Subtract(new TimeSpan(10, 0, 0, 0))) && DateTime.Today < (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)))
        //    {
        //        if (Notification_Project.TestCases.Count == 0)
        //        {
        //                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases<font color='red'> missing </font>yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /><br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
        //                string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                Notification_Project.Session.BeginTransaction();
        //                Notification_Project.SecondEmailSent = true;
        //                Notification_Project.Save();
        //                Notification_Project.Session.CommitTransaction();

        //        }
        //        else
        //        {
        //            bool sendmail = false;
        //           // int isnotoriginal = 0;
        //            string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //            Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
        //            foreach (TestCase testcase in Notification_Project.TestCases)
        //            {

        //                if (testcase.Status != TestCase.status.Ready)
        //                {

        //                    Emailbody += string.Format("'{0}' status is not ready",testcase.Title);
        //                    sendmail = true;
        //                }

        //            }
        //            if (sendmail == true)
        //            {
        //                string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                Notification_Project.Session.BeginTransaction();
        //                Notification_Project.SecondEmailSent = true;
        //                Notification_Project.Save();
        //                Notification_Project.Session.CommitTransaction();
        //            }
        //            //else if (sendmail == false && isnotoriginal == Notification_Project.TestCases.Count)
        //            //{
        //            //    Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));

        //            //    string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //            //    SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //            //}
        //        }
        //    }
        //    else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)))
        //    {
        //        if (Notification_Project.TestCases.Count == 0)
        //        {
        //                string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //                Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
        //                string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                Notification_Project.Session.BeginTransaction();
        //                Notification_Project.SecondEmailSent = true;
        //                Notification_Project.Save();
        //                Notification_Project.Session.CommitTransaction();
        //        }
        //        else
        //        {
        //            bool sendmail = false;
        //            string Emailbody = " Dear " + Notification_Project.ProjectOwner.Name + "<br /><br />";
        //            Emailbody += string.Format("@In reference to the project '" + Notification_Project.Name + "' , please note that you have some test cases not ready yet and you have only '{0}' remaining days to complete it before the end of the related project phase which will be end on Sunday, {1} <br /> Details of the object(s) not ready: <br /><br />", ((CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).Subtract(DateTime.Today)).Days, (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)));
        //            foreach (TestCase testcase in Notification_Project.TestCases)
        //            {

        //                if (testcase.Status != TestCase.status.Ready)
        //                {
        //                    Emailbody += string.Format(testcase.Title);
        //                    sendmail = true;
        //                }
        //            }
        //            if (sendmail == true)
        //            {
        //                string TOEmail = Notification_Project.ProjectOwner.CurrentEmailAddress + ",quality@ariasystems.biz";
        //                SendEmail(Emailbody, "BaselineForTestCases ", TOEmail);
        //                Notification_Project.Session.BeginTransaction();
        //                Notification_Project.SecondEmailSent = true;
        //                Notification_Project.Save();
        //                Notification_Project.Session.CommitTransaction();
        //            }
        //        }
        //    }
        //    else if (DateTime.Today == (CalcEndate.CalcEnddate(Notification_Project.StartDate, 3)).AddDays(1))
        //    {
        //        if (Notification_Project.TestCases.Count == 0)
        //        {

        //            CreateNCs((int)QANonComplains.NonComplianceItems.TestCases, "test cases is missing ", new Session(), Notification_Project);
        //        }
        //        else
        //        {
        //            bool sendmail = false;
        //            string ncdesc = " ";
        //            foreach (TestCase testcase in Notification_Project.TestCases)
        //            {
        //                if (testcase.Status != TestCase.status.Ready)
        //                {
        //                    sendmail = true;
        //                    ncdesc += string.Format("\"{0}\" status is not ready ", testcase);
        //                }
        //                if (sendmail)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.TestCases, ncdesc, new Session(), Notification_Project);
        //                }
        //            }
        //        }
        //    }
        //    //  }
        //    //  }
        //}
        //public static void FillEmailBody(string emailbody, ProjectTemplate Project, bool createNc)
        //{
        //    string body = "FillEmailBody start work now";
        //    string emailto = "ahmed.t@ariany.com";
        //    SendEmail(body, "test mail", emailto);
        //    bool requirement = false;
        //    bool uc = false;
        //    bool pe = false;
        //    bool ucp = false;
        //    if (Project.Requirements.Count == 0)
        //    {
        //        emailbody += "@Requirements is <font color='red'> missing </font> <br />";
        //        requirement = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, "is missing", new Session(), Project);
        //        }
        //    }
        //    else
        //    {
        //        foreach (Requirement existrequirement in Project.Requirements)
        //        {
        //            if (existrequirement.RequirementStatus != Requirement.RequirmentsStatus.Ready)
        //            {
        //                emailbody += string.Format("Requirement : '{0}' it's Status is not ready <br />", existrequirement.Title);
        //                requirement = true;
        //                if (createNc == true)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.Requirement, string.Format(" \"{0}\" status is not ready", existrequirement.Title), new Session(), Project);
        //                }
        //            }
        //        }
        //    }
        //    if (Project.ProjectEntities.Count == 0)
        //    {
        //        emailbody += "@Project entities is <font color='red'> missing  </font><br />";
        //        pe = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.ProjectEntities, "is missing", new Session(), Project);

        //        }
        //    }
        //    else
        //    {
        //        foreach (QAProjectEntity existprojectenity in Project.ProjectEntities)
        //        {
        //            if (existprojectenity.ProjectEntity_Statues != QAProjectEntity.Statues.Ready)
        //            {
        //                emailbody += string.Format("Project Entity : '{0}' it's status is not ready<br />", existprojectenity.Name);
        //                pe = true;
        //                if (createNc == true)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.ProjectEntities, string.Format(" \"{0}\" status is not ready", existprojectenity.Name), new Session(), Project);

        //                }
        //            }
        //        }
        //    }
        //    if (Project.UseCasePoints == null)
        //    {
        //        emailbody += "@Use Case Points is <font color='red'> missing </font><br />";
        //        ucp = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.UseCasePoints, "is missing ", new Session(), Project);
        //        }
        //    }
        //    if (Project.UseCases.Count == 0)
        //    {
        //        emailbody += "@Use Cases is <font color='red'> missing </font> <br />";
        //        uc = true;
        //        if (createNc == true)
        //        {
        //            CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, "is missing", new Session(), Project);
        //        }
        //    }
        //    else
        //    {
        //        foreach (QAUseCase existusecase in Project.UseCases)
        //        {
        //            if (existusecase.UseCase_Statues != QAUseCase.Statues.Ready)
        //            {
        //                emailbody += string.Format("Use Case : '{0}' it's status is not ready<br />", existusecase.Name);
        //                uc = true;
        //                if (createNc == true)
        //                {
        //                    CreateNCs((int)QANonComplains.NonComplianceItems.UseCases, string.Format("\"{0}\" status is not ready", existusecase.Name), new Session(), Project);

        //                }
        //            }
        //        }
        //    }
        //    if (requirement || pe || uc || ucp)
        //    {

        //        string ToEmail = Project.ProjectOwner.CurrentEmailAddress;
        //        SendEmail(emailbody, "BaseLineNotification", ToEmail);
        //        Project.Session.BeginTransaction();
        //        Project.EmailSent = true;
        //        Project.Save();
        //        Project.Session.CommitTransaction();
        //    }
        //}
        //public static void SendEmail(string emailbody, string emailtitle, string emailTo)
        //{
        //    Email notificationmail = new Email();
        //    notificationmail.FromEmail = "khaled.m@ariasystems.biz";
        //    notificationmail.EmailPassword = "Kamag@2016";
        //    notificationmail.ToEmail = emailTo.ToString();
        //    emailbody = emailbody.Replace("@", System.Environment.NewLine);
        //    notificationmail.EmailBody = emailbody;
        //    notificationmail.EmailTitle = emailtitle;
        //    notificationmail.EmailSubject = "Base line Notification ";
        //    notificationmail.SendEmail();
        //}
        //public static void CreateNCs(int Object, string desc, Session Objectspace, ProjectTemplate project)
        //{
        //    Objectspace.ConnectionString = connectionstr;
        //    Objectspace.Connect();
        //    if (Object == (int)QANonComplains.NonComplianceItems.TestCases)
        //    {
        //        Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','8A46A928-AE14-4C48-95DF-E226E256D695','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
        //    }
        //    else
        //    {
        //        Objectspace.ExecuteQuery("insert into QANonComplains ([oid],[Owner],[RelatedProject],[Description],[NonComplainType],[Status],[NonComplianceObjects],[AlarmTime],[NotificationMessage]) Values ('" + Guid.NewGuid() + "','" + project.ProjectOwner.Oid + "','" + project.Oid + "','" + desc + "','cbca1979-ec87-4f95-9321-c8b28c1bb177','" + Object + "','" + 1 + "','" + DateTime.Today + "','" + string.Format("Kindly note that you have Nc related to this project \"{0}\"", project.Name) + "')");
        //    }
        //    Objectspace.Disconnect();
        //}

        //MMT
        public static void insertIndexsetting()
        {
            // byte[] xxxx = File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx");
            //
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx"));
            //
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\sydindexfields(AriaIndex).xlsx"));
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFile(Data)SeetingTypes.xlsx"));
            // 
            //
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 5; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                //
                //ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaField'"));
                ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaIndex'"));
                //
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {
                    if (type != null)
                    {
                        SettingType seetingtype = type.SettingTypes.Where(x => x.SettingTypeId.ToUpper() == dataTable.Rows[i][0].ToString().ToUpper()).FirstOrDefault();
                        if (seetingtype != null)
                        {
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                        }
                        else
                        {
                            seetingtype = new SettingType(sess);
                            seetingtype.SettingTypeId = dataTable.Rows[i][0].ToString().Trim();
                            seetingtype.Name = dataTable.Rows[i][3].ToString();
                            seetingtype.DataType = (SettingType.EnumDataType)Enum.Parse(typeof(SettingType.EnumDataType), dataTable.Rows[i][1].ToString());
                            seetingtype.Width = short.Parse(dataTable.Rows[i][2].ToString());
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                            seetingtype.ObjectTypes.Add(type);
                        }
                        seetingtype.Save();
                    }

                }
                //sess.CommitTransaction();
            }
        }
        //XXX
        public static void insertMenuSetting()
        {
            // byte[] xxxx = File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx");
            //
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFieldSeetingTypes.xlsx"));
            //
            MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\sycmenufields.xlsx"));
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\AriaFile(Data)SeetingTypes.xlsx"));
            // 
            //
            long length = ms.Length;
            byte[] buffer = ms.ToArray();
            ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            IWorkbook workbook = xsheet.Document;

            workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //workbook.Worksheets.Remove(workbook.Worksheets["Sheet1"]);
            // string filename = File.FileName.Substring(0,File.FileName.Length-4);
            Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            int row = WorkSheet.Rows.LastUsedIndex;
            //   Range range = WorkBook.GetDataRange();
            DataTable dataTable = new DataTable();// first.CreateDataTable(range, true);
            for (int y = 1; y <= row; y++)
            {
                Row item = WorkSheet.Rows[y];

                int rn = y + 1;
                dataTable.Rows.Add();
                for (int i = 0; i < 5; i++)
                {
                    int dd = i + 1;
                    dataTable.Columns.Add();
                    dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

                }

            }
            if (dataTable.Rows.Count > 0)
            {
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                //
                //ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'AriaField'"));
                ObjectType type = sess.FindObject<ObjectType>(CriteriaOperator.Parse("ObjectTypeID = 'Menu'"));
                //
                for (int i = 0; i < dataTable.Rows.Count; i++)
                {
                    if (type != null)
                    {
                        SettingType seetingtype = type.SettingTypes.Where(x => x.SettingTypeId.ToUpper() == dataTable.Rows[i][0].ToString().ToUpper()).FirstOrDefault();
                        if (seetingtype != null)
                        {
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                        }
                        else
                        {
                            seetingtype = new SettingType(sess);
                            seetingtype.SettingTypeId = dataTable.Rows[i][0].ToString().Trim();
                            seetingtype.Name = dataTable.Rows[i][2].ToString();
                            seetingtype.DataType = (SettingType.EnumDataType)Enum.Parse(typeof(SettingType.EnumDataType), dataTable.Rows[i][1].ToString());
                            seetingtype.Width = short.Parse(dataTable.Rows[i][3].ToString());
                            seetingtype.ActualColumnName = dataTable.Rows[i][0].ToString();
                            seetingtype.ObjectTypes.Add(type);
                        }
                        seetingtype.Save();
                    }

                }
                //sess.CommitTransaction();
            }
        }
        //XXX
        public static void MapIndexToentities()
        {
            //MemoryStream ms = new MemoryStream(System.IO.File.ReadAllBytes(@"D:\Aria4xpTablesentities.xlsx"));
            //long length = ms.Length;
            //byte[] buffer = ms.ToArray();
            //ASPxSpreadsheet xsheet = new ASPxSpreadsheet();
            //IWorkbook workbook = xsheet.Document;

            //workbook.LoadDocument(buffer, DocumentFormat.Xlsx);
            //Worksheet WorkSheet = workbook.Worksheets.ActiveWorksheet;
            //int row = WorkSheet.Rows.LastUsedIndex;
            //DataTable dataTable = new DataTable();
            //for (int y = 1; y <= row; y++)
            //{
            //    Row item = WorkSheet.Rows[y];

            //    int rn = y + 1;
            //    dataTable.Rows.Add();
            //    for (int i = 0; i < 3; i++)
            //    {
            //        int dd = i + 1;
            //        dataTable.Columns.Add();
            //        dataTable.Rows[y - 1][i] = WorkSheet[y, i].Value;

            //    }

            //}
            //if (dataTable.Rows.Count > 0)
            //{
                Session sess = new Session();
                sess.ConnectionString = connectionstr;
                sess.Connect();
                ObjectType entitytype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'Ariafile'"));
                ObjectType Filetype = sess.FindObject<ObjectType>(CriteriaOperator.Parse("[ObjectTypeID] = 'AriaIndex'"));
                //
                System.Collections.ICollection Indexes =sess.GetObjects (sess.GetClassInfo<AriaObject>(), CriteriaOperator.Parse("[ObjectType] = '" + Filetype.Oid + "'"), new SortingCollection(), 10000, false , true);
                //
                foreach (AriaObject datarow in Indexes)
                {
                if (datarow.ObjectName.Trim().ToUpper() == "ORDACCT")
                { }
                try
                {
                    var mfileName = datarow.AriaObjectSettings.Where(a => a.ActualColumnName != null && a.ActualColumnName.Trim().ToUpper() == "CFILE_NAM").FirstOrDefault ();
                    var value = mfileName.Value ;
                   // AriaObjectSetting fileName = datarow.AriaObjectSettings.FirstOrDefault(r => r.ActualColumnName!= null & r.ActualColumnName.Trim ().ToUpper() == "CFILE_NAM");

                    AriaObject Filerecord = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectType] = '" + entitytype.Oid + "' and [ObjectName]= '" + value.Trim () + "'"));
                    if (Filerecord != null)
                    {
                        //AriaObject Entity = sess.FindObject<AriaObject>(CriteriaOperator.Parse("[ObjectType] = '" + entitytype.Oid + "' and [ObjectName]= '" + datarow[1].ToString() + "'"));
                        //if (Entity != null)
                        // {
                        datarow.ParentObjectID = Filerecord.ParentObjectID;
                        datarow.Save();
                        //}
                    }
                }
                catch(Exception e) { }
                }
            }

        //}



        //MMT
    }
}
