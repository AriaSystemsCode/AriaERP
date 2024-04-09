using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using System.Data.Odbc;
using System.Xml;
using System.Collections;
using Aria.Utilities.Aria40Converter.Helpers;
using System.Windows.Forms;

namespace Aria.Utilities.Aria40Converter.TestProject
{
    /// <summary>
    /// Summary description for AriaDataObjectPointerAdapters
    /// </summary>
    [TestClass]
    public class AriaDataObjectPointerAdapters
    {
        public AriaDataObjectPointerAdapters()
        {
            //
            // TODO: Add constructor logic here
            //
        }

        private TestContext testContextInstance;

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext
        {
            get
            {
                return testContextInstance;
            }
            set
            {
                testContextInstance = value;
            }
        }

        #region Additional test attributes
        //
        // You can use the following additional attributes as you write your tests:
        //
        // Use ClassInitialize to run code before running the first test in the class
        // [ClassInitialize()]
        // public static void MyClassInitialize(TestContext testContext) { }
        //
        // Use ClassCleanup to run code after all tests in a class have run
        // [ClassCleanup()]
        // public static void MyClassCleanup() { }
        //
        // Use TestInitialize to run code before running each test 
        // [TestInitialize()]
        // public void MyTestInitialize() { }
        //
        // Use TestCleanup to run code after each test has run
        // [TestCleanup()]
        // public void MyTestCleanup() { }
        //
        #endregion

        [TestMethod]
        public void TestMethod1()
        {
            //
            // TODO: Add test logic	here
            //
        }



        public List<string> GetPaths(ArrayList xx, List<string> pathes, string parentObject)
        {
            foreach (AriaDataPath DataPath in xx)
            {
                if (DataPath.ChildDataPaths.Count == 0)
                {
                    pathes.Add(parentObject + "." + DataPath.DataPath);
                }
                else
                    GetPaths(DataPath.ChildDataPaths, pathes, parentObject + "." + DataPath.DataPath.RemoveSpecialChar());
            }

            return pathes;
        }



        [TestMethod()]
        public void AriaDataObjectPointerAdapter()
        {
            AriaDbConnection connection = new AriaDbConnection();
            connection.CompanyName = "AKA10";

            AriaDbCommand command7 = new AriaDbCommand("Select ObjectName  FROM AriaObject where ObjectName like 'Aria4Xp%' AND ObjectType = 'RelatedData' ", connection, Aria.Environment.AriaDatabaseTypes.Aria50SystemFiles, "");
            command7.ExecuteNonQuery();
            List<DataRow> listOfObjects = command7.GetDataTable().AsEnumerable().ToList();

            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            DataTable table = new DataTable();
            table.Columns.Add("No.");
            table.Columns.Add("Object Name");
            table.Columns.Add("Data Path");
            table.Columns.Add("Result");
            table.Columns.Add("Error Message");
            DataSet dt = new DataSet();
            dt.Tables.Add(table);

            SaveFileDialog dialog = new SaveFileDialog();
            if (MessageBox.Show("Do you like to save AriaDataObjectPointerAdapters", "Save", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
                dialog.DefaultExt = "xml";
                if (dialog.ShowDialog() == DialogResult.OK)
                {
                    dt.WriteXml(dialog.FileName);
                }
            }

            ArrayList ariaobjects = objectDictionary.LoadAriaObjects(connection, "AKA10");

            foreach (AriaObject ariaObject in ariaobjects)
            {
                if (ariaObject.ObjectName.StartsWith("Aria4XP.") && ariaObject.ObjectType == AriaObjectTypes.Data && ariaObject.ObjectName.Split('.').Count() == 2)
                {
                    List<string> Pathes = new List<string>();
                    List<string> PK = new List<string>();
                    List<string> listOfPathes = new List<string>();
                    int i = 0;
                    string Error = "";


                    AriaDataObjectPointerAdapter AriaDataObjectPointer = default(AriaDataObjectPointerAdapter);
                    AriaDataObjectPointerSettings pointerSettings = new AriaDataObjectPointerSettings();

                    pointerSettings.DataObjectName = ariaObject.ObjectName;
                    pointerSettings.DataObjectRevision = ariaObject.ActiveRevision;

                    AriaDataObjectPointer pointerData = new AriaDataObjectPointer();


                    try
                    {
                        Aria.EnterpriseServices.ObjectDictionary.AriaObjectDataPathsExplorer x = new AriaObjectDataPathsExplorer();
                        ArrayList PathTree = x.GetDataPathsTree(ariaObject.ObjectName, "AKA10");

                        Pathes = GetPaths(PathTree, Pathes, ariaObject.ObjectName);

                        ArrayList ariaObjectProp = objectDictionary.LoadAriaObjectProperties(connection, ariaObject.ObjectName, "001.000", false, "");
                        ArrayList ariaObjectRelatedProp = objectDictionary.LoadAriaObjectRelatedProperties(connection, ariaObject.ObjectName, "001.000", false, "");
                        AriaObjectRevision revision = objectDictionary.LoadAriaObjectRevision(connection, ariaObject.ObjectName, "001.000", "");
                        AriaDataObjectSettings ariaobjectRevision = (AriaDataObjectSettings)objectDictionary.LoadAriaObjectRevision(connection, ariaObject.ObjectName, "001.000", "").ObjectRevisionSettings;

                        foreach (AriaObjectProperty pro in ariaObjectProp)
                        {
                            if (((AriaFieldSettings)pro.PropertySettings).IsPrimaryKey == true)
                            {
                                PK.Add(((AriaFieldSettings)pro.PropertySettings).FieldName);
                            }
                        }

                        AriaDbCommand command88 = new AriaDbCommand("Select  *  FROM " + ariaobjectRevision.TableName, connection, ariaobjectRevision.DatabaseType, "AKA10");
                        command88.ExecuteNonQuery();

                        DataTable selectedtable = command88.GetDataTable();

                        foreach (string key in PK)
                        {
                            try
                            {
                                pointerData.AddKeyField(key, selectedtable.Rows[0][key.Trim()].ToString().Trim());
                            }
                            catch (Exception e)
                            {
                                string cc = e.Message;
                            }
                        }

                        try
                        {
                            AriaDataObjectPointer = new AriaDataObjectPointerAdapter(connection, pointerSettings, pointerData, "AKA10");
                            i = AriaDataObjectPointer.LoadedRecords.Count;
                        }
                        catch (Exception e)
                        {
                            Error = e.Message;
                            i = 0;
                        }

                    }
                    catch (Exception e)
                    {
                        Error = e.Message;
                        i = 0;
                    }
                    finally
                    {
                        if (i == 0)
                        {
                            table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, "", 0, Error);
                        }
                        else
                        {
                            foreach (string path in Pathes)
                            {
                                try
                                {
                                    AriaDataObjectPointer.GetData(path, "AKA10").ToString();
                                }
                                catch (Exception e)
                                {
                                    Error = e.Message;
                                    i = 0;
                                }
                                finally
                                {
                                    if (i == 0)
                                        table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, path, AriaDataObjectPointer.GetData(path, "AKA10").ToString(), Error);
                                    else
                                        table.Rows.Add(ariaObject.ObjectID, ariaObject.ObjectName, path, 0, Error);
                                }

                            }
                        }
                        dt.WriteXml(dialog.FileName);
                    }

                }

            }


            //Assert.IsTrue(table.Tables[0].Rows.Count == 0);

            Assert.Inconclusive("Verify the correctness of this no AriaDataObjectPointerAdapters.");
        }

    }
}
