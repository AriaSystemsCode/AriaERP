using Aria5.DevExpress.SystemAdmin.WebServiceRole.Mangers;
using CopyDBDataFromSQLiteToSQLServer.ServiceReferenceSync;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data;
using System.Data.Common;
using System.Data.SqlClient;
using System.Data.SQLite;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Forms;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using TopoSortDemo;

namespace CopyDBDataFromSQLiteToSQLServer
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            DataContext = this;
        }

        private SQLiteConnection connectionSqlite;
        private SqlConnection connectionSqlServer;

        private Dictionary<string, List<Column>> _columnListSqlite = new Dictionary<string, List<Column>>();
        public Dictionary<string, List<Column>> ColumnListSqlite
        {
            set
            {
                _columnListSqlite = value;
            }
            get
            {
                return _columnListSqlite;
            }
        }

        private Dictionary<string, List<Column>> _columnListSqlServer = new Dictionary<string, List<Column>>();
        public Dictionary<string, List<Column>> ColumnListSqlServer
        {
            set
            {
                _columnListSqlServer = value;
            }
            get
            {
                return _columnListSqlServer;
            }
        }


        private string connectionStringSqlite = "Data Source=";
        private string connectionStringSqlServer = "";


        private ObservableCollection<string> _tableNamesSqlite = new ObservableCollection<string>();
        public ObservableCollection<string> TableNamesSqlite
        {
            set
            {
                _tableNamesSqlite = value;
            }
            get
            {
                return _tableNamesSqlite;
            }
        }

        private ObservableCollection<string> _tableNamesSqlServer = new ObservableCollection<string>();
        public ObservableCollection<string> TableNamesSqlServer
        {
            set
            {
                _tableNamesSqlServer = value;
            }
            get
            {
                return _tableNamesSqlServer;
            }
        }


        private ObservableCollection<string> _matchedTableNames = new ObservableCollection<string>();
        public ObservableCollection<string> MatchedTableNames
        {
            set
            {
                _matchedTableNames = value;
            }
            get
            {
                return _matchedTableNames;
            }
        }

        private ObservableCollection<string> _missMatchedCapTableNames = new ObservableCollection<string>();
        public ObservableCollection<string> MissMatchedCapTableNames
        {
            set
            {
                _missMatchedCapTableNames = value;
            }
            get
            {
                return _missMatchedCapTableNames;
            }
        }

        private Dictionary<string, List<Column>> _missMatchedCapColumnNames = new Dictionary<string, List<Column>>();
        public Dictionary<string, List<Column>> MissMatchedCapColumnNames
        {
            set
            {
                _missMatchedCapColumnNames = value;
            }
            get
            {
                return _missMatchedCapColumnNames;
            }
        }

        private Dictionary<string, List<Column>> _missingColumnNames = new Dictionary<string, List<Column>>();
        public Dictionary<string, List<Column>> MissingColumnNames
        {
            set
            {
                _missingColumnNames = value;
            }
            get
            {
                return _missingColumnNames;
            }
        }

        private void ButtonConnection_Click(object sender, RoutedEventArgs e)
        {
            
            ButtonBrowseDB.IsEnabled = false;
            connectionSqlite = new SQLiteConnection(connectionStringSqlite);




            try
            {
                connectionSqlite.OpenAsync();
                DataTable tables = connectionSqlite.GetSchema("Tables");
                DataTable columns = connectionSqlite.GetSchema("Columns");

                GetColumns(ColumnListSqlite, TableNamesSqlite, tables, columns);

                //GetData();


            }
            catch (SQLiteException ex)
            {
                System.Windows.MessageBox.Show(ex.Message);
                connectionSqlite.Close();
            }

            //List<ObjectData> listAccount = await GetData("Account");
            //Guid accountOid = Guid.Parse(listAccount[0].Values[0].ToString());
            Guid accountOid = Guid.Parse("{df0f09dc-256d-4459-95b3-64d1347555a4}");

            //ServiceReferene1.AriaSynchronizationServiceSoapClient x1 = new ServiceReferenServiceReference1.AriaSynchronizationServiceSoapClient();
            //var result2= x1.GetDBVersion(accountOid);
            //var result1 = x1.PullData(accountOid, 0, 20, "");
            //sara [Start]
            PrepareData("Language", accountOid, -1);
            PrepareData("Account", accountOid, -1);

            PrepareData("AccountLocalization", accountOid, -1);

            //sara [End]
            PrepareData("EntityType", accountOid, -1);
            PrepareData("GuideType", accountOid, -1);
            PrepareData("GuideTypeEntityType", accountOid, -1);
            PrepareData("EntityCategory", accountOid, 4);
            PrepareData("EntityCategoryLocalization", accountOid, -1);
            PrepareData("EntityStatus", accountOid, -1);
            PrepareData("EntityStatusLocalization", accountOid, -1);
            PrepareData("EntityClassification", accountOid, 4);
            PrepareData("EntityClassificationLocalization", accountOid, -1);
            PrepareData("Entity", accountOid, -1);
           

            PrepareData("EntityLocalization", accountOid, -1);
            PrepareData("Profile", accountOid, -1);        //x
            PrepareData("ProfileGroup", accountOid, -1); //x
            PrepareData("ProfileGroupTile", accountOid, -1);//x
            //  await PrepareData("Account", accountOid, -1);
            PrepareData("ProfileSetting", accountOid, -1);
            //await PrepareData("IndoorMap", accountOid, -1);
            //await PrepareData("IndoorMapProvider", accountOid, -1);
            // PrepareData("Language", accountOid, -1); //x
            PrepareData("LocationType", accountOid, -1);

            PrepareData("GeneralSetting", accountOid, -1);
            PrepareData("EntityTypeSettings", accountOid, -1);
            PrepareData("EntityTypesRelationship", accountOid, -1);
            PrepareData("EntityTypesRelationshipLocalization", accountOid, -1);
            //await PrepareData("Contact", accountOid, -1);
            PrepareData("DataFilter", accountOid, -1);
            PrepareData("DataFilterColumn", accountOid, -1);
            PrepareData("DataSort", accountOid, -1);
            PrepareData("DataSortColumn", accountOid, -1);
            PrepareData("EntitiesRelationship", accountOid, -1);

            //await PrepareData("AccountLocalization", accountOid, -1);
            PrepareData("AttachmentCategory", accountOid, -1);
            PrepareData("Attachments", accountOid, -1);
            PrepareData("EntityAttachment", accountOid, -1);
            //await PrepareData("EntityFts", accountOid, -1);


            ButtonBrowseDB.IsEnabled = true;


        }

        public void PrepareData(string tableName, Guid accountOid, int index)
        {
            List<ObjectData> dataList = GetData(tableName);
            List<Item> items = new List<Item>();

            foreach (var data in dataList)
            {
                items.Add(new Item(data.Values[0].ToString()));
            }

            foreach (var item in items)
            {
                ObjectData dataobj = dataList.First(r => r.Values[0].ToString() == item.Name);

                if (index >= 0 && dataobj.Values[index].ToString() != "" && Guid.Parse(dataobj.Values[index].ToString()) != Guid.Empty)
                {
                    item.Dependencies = new Item[] { items.First(r => r.Name == dataobj.Values[4].ToString()) };
                }

                Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject syncObj = new Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject();
                syncObj.AccountOID = accountOid;
                syncObj.EntityOID = Guid.Parse(dataobj.Values[0].ToString());
                syncObj.TableName = tableName;
                syncObj.RecordStatus = "Add";
                syncObj.Version = "";
                syncObj.Properties = new ServiceReferenceSync.ArrayOfString();
                syncObj.Values = new ServiceReferenceSync.ArrayOfAnyType();
                foreach (string prop in dataobj.Properties)
                {
                    syncObj.Properties.Add(prop);
                }
                foreach (var val in dataobj.Values)
                {
                    syncObj.Values.Add(val);
                }
                item.syncObjList = new List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>();
                
                item.syncObjList.Add(syncObj);
            }

            SyncManager x = new SyncManager();

            // var sorted = TopologicalSort.Sort(items, x => x.Dependencies);
            // ServiceReferenceSync.AriaSynchronizationServiceSoapClient a = new ServiceReferenceSync.AriaSynchronizationServiceSoapClient();

            List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject> dataobjects1 = new List<Aria5.DevExpress.SystemAdmin.WebServiceRole.Marshalling.SyncDataObject>();
            foreach (var item in items)
            {
                dataobjects1.Add(item.syncObjList.ToArray().First());
            }
            x.PushData(accountOid, dataobjects1, "");

            //foreach (var item in items)
            //{
            //    x.PushData(accountOid, dataobjects1, "");
            //}
        }
        private async void GetData()
        {

            foreach (var table in ColumnListSqlite)
            {
                string cmd = "SELECT ";
                for (int i = 0; i < table.Value.Count; i++)
                {
                    cmd += "[" + table.Value[i].ColumnName + "]";
                    if (i < (table.Value.Count - 1))
                    {
                        cmd += ", ";
                    }
                }
                cmd += " FROM [" + table.Key + "];";

                SQLiteCommand sqliteCommand = new SQLiteCommand(cmd, connectionSqlite);
                DbDataReader reader = await sqliteCommand.ExecuteReaderAsync();

                while (await reader.ReadAsync())
                {
                    foreach (Column column in table.Value)
                    {
                        if (column.ColumnName != "TransactionTimeStamp")
                        {
                            int order = reader.GetOrdinal(column.ColumnName);
                            var x = reader[order];
                        }
                    }

                }
            }

        }


        private List<ObjectData> GetData(string tableName)
        {
            var table = ColumnListSqlite.FirstOrDefault(r => r.Key == tableName);
            //List<Column> columns = ColumnListSqlite.FirstOrDefault(r => r.Key == tableName).Value;
            string cmd = "SELECT ";
            for (int i = 0; i < table.Value.Count; i++)
            {
                cmd += "[" + table.Value[i].ColumnName + "]";
                if (i < (table.Value.Count - 1))
                {
                    cmd += ", ";
                }
            }
            cmd += " FROM [" + table.Key + "];";

            SQLiteCommand sqliteCommand = new SQLiteCommand(cmd, connectionSqlite);
            DbDataReader reader = sqliteCommand.ExecuteReader();
            List<ObjectData> dataList = new List<ObjectData>();
            while (reader.Read())
            {
                ObjectData data = new ObjectData();
                foreach (Column column in table.Value)
                {
                    if (column.ColumnName != "TransactionTimeStamp")
                    {
                        int order = reader.GetOrdinal(column.ColumnName);
                        data.Properties.Add(column.ColumnName);
                        data.Values.Add(reader[order]);

                        //var x = reader[order];
                    }
                }
                dataList.Add(data);

            }

            return dataList;
        }
        private void GetColumns(Dictionary<string, List<Column>> ColumnList, ObservableCollection<string> TableList, DataTable tables, DataTable columns)
        {
            foreach (DataRow row in tables.Rows)
            {
                TableList.Add(row[2].ToString());
            }

            foreach (DataRow row in columns.Rows)
            {
                if (!ColumnList.Keys.Contains(row[2].ToString()))
                {
                    ColumnList.Add(row[2].ToString(), new List<Column>());
                }

                Column col = new Column();
                col.ColumnName = row[3].ToString();
                col.DataType = row[11].ToString();
                col.TableName = row[2].ToString();
                ColumnList[row[2].ToString()].Add(col);
            }
        }

        private void ButtonBrowseDB_Click(object sender, RoutedEventArgs e)
        {
            OpenFileDialog fileDialog = new OpenFileDialog();
            //fileDialog.InitialDirectory = @"C:\Users\mena\AppData\Local\Packages\B6A606E0.Resto_fw0mw1jjr845t\LocalState\Demos";
            fileDialog.InitialDirectory = @"D:\";
            fileDialog.Multiselect = false;

            DialogResult result = fileDialog.ShowDialog();
            if (result == System.Windows.Forms.DialogResult.OK)
            {
                TextBlockBrowseDB.Text = fileDialog.FileName;
                connectionStringSqlite += fileDialog.FileName + ";";
            }
        }

        private async void ButtonConnectionAzure_Click(object sender, RoutedEventArgs e)
        {
            ButtonConnectionAzure.IsEnabled = false;
            ProgressBarLoadProgress.IsIndeterminate = true;
            try
            {
                //if (ButtonLiveDB.IsChecked == true)
                //{
                //    connectionStringSqlServer = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin;User ID=azuresqladmin;Password=aria_123";
                //    //XpoDefault.Session.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin;User ID=azuresqladmin;Password=aria_123";
                //}
                //else 
                if (ButtonStagingDB.IsChecked == true)
                {
                    connectionStringSqlServer = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
                    //XpoDefault.Session.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
                }
                else
                    if (ButtonLocalDB.IsChecked == true)
                    {
                        connectionStringSqlServer = @"Data Source=NSDE_MINA;Initial Catalog=Aria5SystemAdminBackup24-09;User ID=sa;Password=aria_123";
                        //connectionStringSqlServer = "Data Source=NSDE_MINA; Initial Catalog=Aria5SystemAdmin_Staging; Integrated Security=SSPI";

                    }

                connectionSqlServer = new SqlConnection(connectionStringSqlServer);
                await connectionSqlServer.OpenAsync();
                Task task = new Task(() =>
                {
                    DataTable tables = connectionSqlServer.GetSchema("Tables");
                    DataTable columns = connectionSqlServer.GetSchema("Columns");

                    GetColumns(ColumnListSqlServer, TableNamesSqlServer, tables, columns);
                });
                task.Start();
                await task;
            }
            catch (Exception ex)
            {
                System.Windows.MessageBox.Show(ex.Message);
            }

            ButtonConnectionAzure.IsEnabled = true;
            ProgressBarLoadProgress.IsIndeterminate = false;
        }


        private void ButtonLocalDB_Checked(object sender, RoutedEventArgs e)
        {

        }

        private void ButtonStagingDB_Checked(object sender, RoutedEventArgs e)
        {
            try
            {
                //XpoDefault.Session.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
            }
            catch (Exception ex)
            {
                System.Windows.MessageBox.Show(ex.Message);
                //ButtonLiveDB.IsChecked = true;
            }
        }


        private void ButtonLiveDB_Checked(object sender, RoutedEventArgs e)
        {
            try
            {
                //XpoDefault.Session.ConnectionString = "Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin;User ID=azuresqladmin;Password=aria_123";
            }
            catch (Exception ex)
            {
                System.Windows.MessageBox.Show(ex.Message);
                //ButtonStagingDB.IsChecked = true;
            }
        }


        private void Compare()
        {
            MatchedTableNames.Clear();
            MissingColumnNames.Clear();
            MissMatchedCapColumnNames.Clear();
            MissMatchedCapTableNames.Clear();
            EqualityComparer comparer = new EqualityComparer();
            foreach (var x in ColumnListSqlite)
            {
                string tableName = x.Key;
                List<Column> cl = x.Value;
                int count = 0;
                int count1 = 0;
                if (ColumnListSqlServer.Keys.Contains(tableName))
                {
                    foreach (Column c in cl)
                    {
                        var result = ColumnListSqlServer[tableName].FirstOrDefault(r => r.ColumnName == c.ColumnName);
                        var result1 = ColumnListSqlServer[tableName].FirstOrDefault(r => r.ColumnName.ToUpper().Trim() == c.ColumnName.ToUpper().Trim());
                        if (result != null)
                        {
                            count++;
                        }
                        else if (c.ColumnName == "TransactionTimeStamp")
                        {
                            count1++;
                        }
                        else if (result1 != null)
                        {
                            if (!MissMatchedCapColumnNames.Keys.Contains(tableName))
                            {
                                MissMatchedCapColumnNames.Add(tableName, new List<Column>());
                            }
                            MissMatchedCapColumnNames[tableName].Add(c);
                        }
                        else
                        {
                            if (!MissingColumnNames.Keys.Contains(tableName))
                            {
                                MissingColumnNames.Add(tableName, new List<Column>());
                            }
                            MissingColumnNames[tableName].Add(c);
                        }

                    }

                    if (count == (x.Value.Count - count1))
                    {
                        MatchedTableNames.Add(tableName);
                    }

                }
                else if (ColumnListSqlServer.Keys.Contains(tableName, comparer))
                {
                    MissMatchedCapTableNames.Add(tableName);
                }


            }
        }

        private void ButtonLoadApps_Click(object sender, RoutedEventArgs e)
        {
            Compare();
            Window1 window1 = new Window1(MatchedTableNames, MissMatchedCapTableNames, MissMatchedCapColumnNames, MissingColumnNames);
            window1.Owner = this;
            window1.Show();




            var a = new Item("A");
            var c = new Item("C");
            var f = new Item("F");
            var h = new Item("H");
            var d = new Item("D", a);
            var g = new Item("G", f, h);
            var e1 = new Item("E", d, g);
            var b = new Item("B", c, e1);

            var unsorted = new[] { a, b, c, d, e1, f, g, h };

            var sorted = TopologicalSort.Sort(unsorted, x => x.Dependencies);


        }




    }
}
