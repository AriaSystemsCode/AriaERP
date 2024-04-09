using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.Data.SqlClient;
using System.Xml;
using System.IO;
using System.Data.OleDb;
using System.Collections;
using System.Xml.Xsl;

namespace Map
{
    struct MapXML
    {
        public static string SrcTable = "SrcTable", SrcField = "SrcField", DestTable = "DestTable", DestField = "DestField";
        public static string Fixed = "Fixed", Condition = "Condition", Usage = "Usage";
        public static string MainSrc = "MainSrc", MainDest = "MainDest";
    }
    public class XmlToSqlClass
    {
        public DataSet dataSetTarget = new DataSet();
        XmlDocument xmlMapdoc = new XmlDocument();
        public DataSet dataSetSource = new DataSet();
        List<string> SourceUsedTables = new List<string>();
        SqlConnection connection = new SqlConnection();
        public string MainSrcTable, MainDestTable, SqlTablesSuffix = "", MainSourceWhere, XSLTFile;
        bool HasMapping, HasSchema, HasXSLT = false;
        Dictionary<string, object> RowDataDictionary = new Dictionary<string, object>();

        /// <summary>
        /// Convert xml file to SQL tables
        /// </summary>
        /// <param name="XMLmappingFile">Mapping file path used to map between input xml and output table</param>
        /// <param name="XSDFile">XSD file path for the input xml</param>
        /// <param name="XMLSource">XML file path to convert to SQL</param>
        /// <param name="SqlServer">SQL Server instance name</param>
        /// <param name="DataBase">SQL Server DataBase name</param>
        /// <param name="UserName">SQL Server Username</param>
        /// <param name="Password">SQL Server Password</param>
        /// <param name="Tables_Prefix">Prefix string to add to SQL server tables names</param>
        /// <param name="where">SQL where statment to filter output SQL tables </param>
        public void XmlToSql(string XMLmappingFile, string XSDFile, string XMLSource, string SqlServer, string DataBase, string UserName, string Password, string Tables_Suffix, string where)
        {
            try
            {
                Error = false;
                HasMapping = XMLmappingFile != null && XMLmappingFile.Trim() != "";
                SqlTablesSuffix = (Tables_Suffix != null && Tables_Suffix.Trim() != "") ? Tables_Suffix : "";
                MainSourceWhere = where;
                FillFromXSD(dataSetSource, XSDFile, XMLSource);

                connection = GetSqlConnection(SqlServer, DataBase, UserName, Password);
                if (HasMapping)
                {
                    xmlMapdoc.Load(XMLmappingFile);
                    AddAliasTables();
                    CreateTablesStructureFromMap(dataSetTarget, "//*/Map/" + MapXML.DestTable, "//*/Map[" + MapXML.DestTable + "='{Table}']/" + MapXML.DestField, dataSetSource, MapXML.SrcTable, MapXML.SrcField);
                    SetRelations();
                    CheckErrors();
                    FillTableData();
                    ContinueCheck();
                    InsertDatasetToSQL(dataSetTarget);
                }
                else
                {
                    InsertDatasetToSQL(dataSetSource);
                }
            }
            catch (Exception ex)
            {
                Error = true;
                ErrorMsg = ex.Message + " " + ex.StackTrace + " " + ex.Data;
            }
            finally
            {
                SqlConnection.ClearAllPools();
            }
        }

        public void ContinueCheck()
        {
            if (ErrorMsgArr.Count > 0)
                throw new Exception("Error Happend cann't continue !!");
        }

        /// <summary>
        /// Fill DataSet with Data and structure from XSD and Xml passed
        /// </summary>
        /// <param name="dataSet">Dataset to fill</param>
        /// <param name="XSDFile">XSD file to read structure from , if empty will get structure from XML file</param>
        /// <param name="XMLSource">XML File to read data from</param>
        private void FillFromXSD(DataSet dataSet, string XSDFile, string XMLSource)
        {
            if (XSDFile != null && XSDFile.Trim() != "")
                dataSet.ReadXmlSchema(XSDFile);
            else
                dataSet.ReadXmlSchema(XMLSource);
            dataSet.ReadXml(XMLSource);
            foreach (DataTable table in dataSet.Tables)
                table.TableName = table.TableName.Trim();
        }

        /// <summary>
        /// Convert SQL tables to xml file  
        /// </summary>
        /// <param name="XMLmappingFile">Mapping file path used to map between input and output</param>
        /// <param name="XSDFile">XSD file path for the xml</param>
        /// <param name="XMLSource">XML file path</param>
        /// <param name="SqlServer">SQL Server instance name</param>
        /// <param name="DataBase">SQL Server DataBase name</param>
        /// <param name="UserName">SQL Server Username</param>
        /// <param name="Password">SQL Server Password</param>
        /// <param name="Tables_Prefix">Prefix string to add to output tables names</param>
        /// <param name="where">SQL where statment to filter output tables </param>
        public void SqlToXml(string XMLmappingFile, string XSDFile, string XMLSource, string SqlServer, string DataBase, string UserName, string Password, string Tables_Suffix, string where)
        {
            try
            {
                Error = false;
                HasMapping = XMLmappingFile != null && XMLmappingFile.Trim() != "";
                HasSchema = XSDFile != null && XSDFile.Trim() != "";
                SqlTablesSuffix = (Tables_Suffix != null && Tables_Suffix.Trim() != "") ? Tables_Suffix : "";
                MainSourceWhere = where;
                if (HasSchema)
                {
                    XSLTFile = Path.Combine(Path.GetDirectoryName(XSDFile), Path.GetFileNameWithoutExtension(XSDFile) + ".xsl");
                    HasXSLT = File.Exists(XSLTFile);
                    XSLTFile = HasXSLT ? XSLTFile : "";
                    dataSetTarget.ReadXmlSchema(XSDFile);
                }
                else
                    dataSetTarget.ReadXmlSchema(XMLSource);

                connection = GetSqlConnection(SqlServer, DataBase, UserName, Password);

                if (HasMapping)
                {
                    xmlMapdoc.Load(XMLmappingFile);
                    fillDataSetFromSQL(dataSetSource, "//*/Map/" + MapXML.SrcTable);
                    AddAliasTables();
                    CreateTablesStructureFromMap(dataSetSource, "//*/Map/" + MapXML.SrcTable, "//*/Map[" + MapXML.SrcTable + "='{Table}']/" + MapXML.SrcField, dataSetTarget, MapXML.DestTable, MapXML.DestField);
                    SetRelations();
                    CheckErrors();
                    FillTableData();
                    ContinueCheck();
                    ToXML(dataSetTarget, XMLSource);
                }
                else
                {
                    InsertDatasetToSQL(dataSetSource);
                }
            }
            catch (Exception ex)
            {
                Error = true;
                ErrorMsg = ex.Message + " " + ex.StackTrace + " " + ex.Data;
            }
        }

        private void CheckErrors()
        {
            if (!dataSetSource.Tables.Contains(MainSrcTable))
                throw new Exception("The Main Source Table " + MainSrcTable + " Not Exits!!");
            if (dataSetSource.Tables[MainSrcTable].Rows.Count == 0)
                throw new Exception("The Main Source Table " + MainSrcTable + " Is Empty!!");

        }

        /// <summary>
        /// Saves passed Dataset as XML in the passed path
        /// </summary>
        /// <param name="dataSetTarget">dataset to save</param>
        /// <param name="FileToSave">File to save.</param>
        private void ToXML(DataSet dataSetTarget, string FileToSave)
        {
            if (HasXSLT)
            {
                XslCompiledTransform myXslTransform = new XslCompiledTransform();
                myXslTransform.Load(XSLTFile);
                //new XsltArgumentList().
                XmlDocument xmldoc = new XmlDocument();
                xmldoc.LoadXml(dataSetTarget.GetXml());
                FileStream fs = File.Create(FileToSave);
                myXslTransform.Transform(xmldoc, null, fs);
                fs.Close();
            }
            else
                dataSetTarget.WriteXml(FileToSave);
        }

        /// <summary>
        /// Fills and creates DataSet tables from SQL tables
        /// </summary>
        /// <param name="dataSet">Dataset to create tables in</param>
        /// <param name="TablesNamesXquery">Xquery which will used to get the tables names from Mapping xml</param>
        private void fillDataSetFromSQL(DataSet dataSet, string TablesNamesXquery)
        {
            Dictionary<string, string> tables = new Dictionary<string, string>();
            foreach (XmlNode table in xmlMapdoc.SelectNodes(TablesNamesXquery))
            {
                if (!tables.Keys.Contains(table.InnerText) && table.InnerText.Trim() != "")
                    tables.Add(table.InnerText, MainSourceWhere);
            }
            dataSet = DataSetFromSQLCreator.Create(dataSet, connection, tables);
        }

        /// <summary>
        /// set Relation on Source and Target dataset according to Map file
        /// </summary>
        private void SetRelations()
        {
            MainSrcTable = xmlMapdoc.SelectSingleNode("//*/" + MapXML.MainSrc).InnerText;
            MainDestTable = xmlMapdoc.SelectSingleNode("//*/" + MapXML.MainDest).InnerText;

            for (int xmlMapIndex = 0; xmlMapIndex < 2; xmlMapIndex++)
            {
                DataSet dataSet = xmlMapIndex == 0 ? dataSetSource : dataSetTarget;
                string DataSettype = xmlMapIndex == 0 ? "Source" : "Destination";

                if (xmlMapdoc.SelectSingleNode("//Relations/" + DataSettype) != null)
                {
                    if (xmlMapdoc.SelectSingleNode("//Relations/" + DataSettype + "/Add") != null)
                    {
                        foreach (XmlNode node in xmlMapdoc.SelectNodes("//Relations/" + DataSettype + "/Add/Relation"))
                        {
                            XmlNodeList parentcolumnsnodes = node.SelectNodes("ParentColumns/Column");
                            DataColumn[] ParentColumns = new DataColumn[parentcolumnsnodes.Count];
                            XmlNodeList childcolumnsnodes = node.SelectNodes("ChildColumns/Column");
                            DataColumn[] ChildColumns = new DataColumn[childcolumnsnodes.Count];
                            DataTable parent = dataSet.Tables[node["ParentTable"].InnerText];
                            DataTable child = dataSet.Tables[node["ChildTable"].InnerText];
                            for (int parentColumnIndex = 0; parentColumnIndex < parentcolumnsnodes.Count; parentColumnIndex++)
                                ParentColumns[parentColumnIndex] = parent.Columns[parentcolumnsnodes[parentColumnIndex].InnerText];
                            for (int childColumnIndex = 0; childColumnIndex < childcolumnsnodes.Count; childColumnIndex++)
                                ChildColumns[childColumnIndex] = child.Columns[childcolumnsnodes[childColumnIndex].InnerText];
                            DataRelation relation = new DataRelation(parent.TableName + "_" + child.TableName, ParentColumns, ChildColumns);
                            parent.ChildRelations.Add(relation);
                        }
                    }
                    if (xmlMapdoc.SelectSingleNode("//Relations/" + DataSettype + "/Remove") != null)
                    {
                        foreach (XmlNode node in xmlMapdoc.SelectNodes("//Relations/" + DataSettype + "/Remove/Relation"))
                        {
                            string parentTable = node["ParentTable"].InnerText;
                            string ChildTable = node["ChildTable"].InnerText;
                            if (dataSet.Tables[parentTable] != null)
                            {
                                foreach (DataRelation relation in dataSet.Tables[parentTable].ChildRelations)
                                {
                                    if (relation.ChildTable.TableName.ToUpper() == ChildTable.ToUpper().Trim())
                                    {
                                        dataSet.Relations.Remove(relation);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Add any source Alias table requested in the Map
        /// </summary>
        private void AddAliasTables()
        {
            foreach (XmlNode node in xmlMapdoc.SelectNodes("//Copy"))
            {
                string maintableName = node["Main"].InnerText;
                string aliastableName = node["Alias"].InnerText;
                DataTable alias = dataSetSource.Tables[maintableName].Copy();
                alias.TableName = aliastableName;
                dataSetSource.Tables.Add(alias);
            }
        }

        /// <summary>
        /// Create Target Tables structure from the Mapping file 
        /// </summary>
        private void CreateTablesStructureFromMap(DataSet TargetDataSet, string TablesNamesXquery, string FieldsNamesXquery, DataSet TypesDataSet, string TypeTableNodeName, string TypeFieldNodeName)
        {
            try
            {
                string currentTable, srctable, srccolumn;
                foreach (XmlNode tableNode in xmlMapdoc.SelectNodes(TablesNamesXquery))
                {
                    currentTable = tableNode.InnerText;
                    if (currentTable.Trim() != "" && !TargetDataSet.Tables.Contains(currentTable))
                    {
                        TargetDataSet.Tables.Add(tableNode.InnerText);
                        foreach (XmlNode fieldNode in tableNode.SelectNodes(FieldsNamesXquery.Replace("{Table}", currentTable)))
                        {
                            srccolumn = fieldNode.ParentNode[TypeFieldNodeName].InnerText;
                            srctable = fieldNode.ParentNode[TypeTableNodeName].InnerText;
                            if (fieldNode.InnerText != "" && !TargetDataSet.Tables[currentTable].Columns.Contains(fieldNode.InnerText))
                                TargetDataSet.Tables[currentTable].Columns.Add(fieldNode.InnerText);
                            if (srctable != "" && srccolumn != "")
                                TargetDataSet.Tables[currentTable].Columns[fieldNode.InnerText].DataType = TypesDataSet.Tables[srctable].Columns[srccolumn].DataType;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                throw new Exception("Error in InitDestTablesStructure :" + ex.Message);
            }
        }

        /// <summary>
        /// Fill data target dataset with data
        /// </summary>
        private void FillTableData()
        {
            FillData(dataSetTarget.Tables[MainDestTable]);
        }

        /// <summary>
        /// Fill passed data table with Data from the XML according to the Map
        /// </summary>
        /// <param name="targetDataTable">Datatable to fill</param>
        private void FillData(DataTable targetDataTable)
        {
            ApplyFilters(dataSetSource);
            int rows = 0;
            Dictionary<string, int> rowsCount = new Dictionary<string, int>();
            bool OneToOne = true, HasFixed = false;
            XmlNodeList Mapnodes = xmlMapdoc.SelectNodes("//*/Map[" + MapXML.DestTable + "='" + targetDataTable.TableName + "']");
            foreach (XmlNode node in Mapnodes)
            {
                if (node[MapXML.SrcTable].InnerText.Trim() != "")
                {
                    string SrcTable = node[MapXML.SrcTable].InnerText, srcField = node[MapXML.SrcField].InnerText;
                    string dicEntry = (SrcTable + "." + srcField).ToUpper();
                    if (!RowDataDictionary.Keys.Contains(dicEntry))
                    {
                        if (!rowsCount.Keys.Contains(SrcTable.ToUpper()))
                        {
                            rows = rows < dataSetSource.Tables[SrcTable].DefaultView.Count ? dataSetSource.Tables[SrcTable].DefaultView.Count : rows;
                            rowsCount.Add(SrcTable.ToUpper(), dataSetSource.Tables[SrcTable].DefaultView.Count);
                        }
                    }
                }
                else if (node["Fixed"].InnerText.Trim() != "")
                    HasFixed = true;
            }

            int lowest = int.MaxValue;
            if (rowsCount.Count == 0)
                lowest = HasFixed ? 1 : 0;

            List<string> parenttables = new List<string>(), childtables = new List<string>();
            foreach (int rowCount in rowsCount.Values)
            {
                if (rowCount > 0 && lowest > rowCount)
                    lowest = rowCount;
            }
            foreach (KeyValuePair<string, int> rowscountEntry in rowsCount)
            {
                if (rowscountEntry.Value == lowest)
                    parenttables.Add(rowscountEntry.Key);
                else if (rowscountEntry.Value > lowest)
                    childtables.Add(rowscountEntry.Key);
            }
            List<DataRelation> ParentToChildRelations = new List<DataRelation>();
            foreach (string childtable in childtables)
            {
                bool RelationFound = false;
                DataRelationCollection childrelations = dataSetSource.Tables[childtable].ParentRelations;
                foreach (string parenttable in parenttables)
                {
                    ParentToChildRelations = new List<DataRelation>();
                    RelationFound = isRelationExist(childrelations, parenttable, ref ParentToChildRelations);
                    if (RelationFound)
                        break;
                }
            }
            OneToOne = childtables.Count == 0;
            DataRowView topRelationRow = null;
            for (int rowNumber = 0; rowNumber < lowest; rowNumber++)
            {
                List<string> ParentAdded = new List<string>();
                string dicEntry = "";
                foreach (string ParentTable in parenttables)
                {
                    foreach (DataColumn parentColumn in dataSetSource.Tables[ParentTable].Columns)
                    {
                        dicEntry = (ParentTable + "." + parentColumn.ColumnName).ToUpper();
                        if (RowDataDictionary.Keys.Contains(dicEntry))
                            RowDataDictionary.Remove(dicEntry);
                        RowDataDictionary.Add(dicEntry, dataSetSource.Tables[ParentTable].DefaultView[rowNumber][parentColumn.ColumnName]);
                        ParentAdded.Add(dicEntry);
                    }
                    if (!OneToOne)
                        if (ParentTable == ParentToChildRelations[0].ParentTable.TableName.ToUpper())
                            topRelationRow = dataSetSource.Tables[ParentTable].DefaultView[rowNumber];
                }
                if (OneToOne)
                    NewRowFromDictionary(targetDataTable);
                else
                {
                    List<DataRow> toprelationrowList = new List<DataRow>();
                    toprelationrowList.Add(topRelationRow.Row);
                    List<DataRow> ChildData = GetChildData(toprelationrowList, ParentToChildRelations);
                    foreach (DataRow childdatarow in ChildData)
                    {
                        List<string> ChildAdded = new List<string>();
                        foreach (DataColumn childcolumn in childdatarow.Table.Columns)
                        {
                            dicEntry = (childdatarow.Table.TableName + "." + childcolumn.ColumnName).ToUpper();
                            if (RowDataDictionary.Keys.Contains(dicEntry))
                                RowDataDictionary.Remove(dicEntry);
                            RowDataDictionary.Add(dicEntry, childdatarow[childcolumn]);
                            ChildAdded.Add(dicEntry);
                        }
                        NewRowFromDictionary(targetDataTable);
                        foreach (string removeItem in ChildAdded)
                            RowDataDictionary.Remove(removeItem);
                    }
                }

                Dictionary<string, string> CurrentFilters = GetCurrentFiltersState(dataSetSource);
                foreach (DataRelation relation in targetDataTable.ChildRelations)
                    FillData(relation.ChildTable);

                foreach (string removeItem in ParentAdded)
                    RowDataDictionary.Remove(removeItem);
                RestoreFilters(dataSetSource, CurrentFilters);
            }
            ClearFilter(dataSetSource);
        }

        /// <summary>
        /// loop through dataset and apply filters to child tables if the parent table have one row only
        /// also adds conditions mentionec in the Map.
        /// </summary>
        /// <param name="dsSource">dataset to apply filters to</param>
        private void ApplyFilters(DataSet dsSource)
        {
            XmlNodeList Mapnodes = xmlMapdoc.SelectNodes("//*/Map");

            Dictionary<string, List<string>> filters = new Dictionary<string, List<string>>();
            bool NotFinished = true;
            List<string> FiltersAdded = new List<string>();
            while (NotFinished)
            {
                NotFinished = false;
                #region Conditions
                foreach (XmlNode node in Mapnodes)
                {
                    if (node["Condition"].InnerText != "")
                    {
                        string condTablestr = node["Condition"]["Table"].InnerText;
                        string condFieldstr = node["Condition"]["Field"].InnerText;
                        string condValuestr = node["Condition"]["Value"].InnerText;
                        condTablestr = condTablestr.ToUpper();
                        condFieldstr = condFieldstr.ToUpper();
                        condValuestr = condValuestr.ToUpper();
                        if (!filters.Keys.Contains(condTablestr))
                            filters.Add(condTablestr, new List<string>());
                        if (!filters[condTablestr].Contains(condFieldstr + " " + condValuestr))
                        {
                            filters[condTablestr].Add(condFieldstr + " " + condValuestr);
                            NotFinished = true;
                        }//dsSource.Tables[condTablestr].DefaultView.RowFilter = condFieldstr + " " + condValuestr;
                    }
                }
                #endregion

                //Apply condition on source
                //if (MainSourceWhere.Trim() != "")
                //{
                //    if (!filters.Keys.Contains(MainSrcTable))
                //        filters.Add(MainSrcTable, new List<string>());
                //    if (!filters[MainSrcTable].Contains(MainSourceWhere))
                //    {
                //        filters[MainSrcTable].Add(MainSourceWhere);
                //        NotFinished = true;
                //    }
                //}

                foreach (DataTable table in dsSource.Tables)
                {
                    string SrcTable = table.TableName.ToUpper();
                    foreach (DataRelation parentRelation in table.ParentRelations)
                    {
                        foreach (DataColumn parentColumns in parentRelation.ParentColumns)
                        {
                            string dicEntry = (parentRelation.ParentTable.TableName + "." + parentColumns.ColumnName).ToUpper();
                            if (RowDataDictionary.Keys.Contains(dicEntry))
                            {
                                string cond = parentColumns.Caption + " = '" + RowDataDictionary[dicEntry] + "'", condsrc = SrcTable.ToUpper();
                                if (!filters.Keys.Contains(condsrc))
                                    filters.Add(condsrc, new List<string>());
                                if (!filters[condsrc].Contains(cond))
                                {
                                    filters[condsrc].Add(cond);
                                    NotFinished = true;
                                }
                                // dsSource.Tables[SrcTable].DefaultView.RowFilter = parentColumns.Caption + " = '" + DataDic[dicEntry] + "'";
                            }
                        }
                    }
                }
                if (filters.Count > 0 && NotFinished)
                    WriteFilters(dsSource, filters, FiltersAdded);
            }

            if (FiltersAdded.Count > 0)
            {
                for (int removecounter = FiltersAdded.Count - 1; removecounter >= 0; removecounter--)
                {
                    RowDataDictionary.Remove(FiltersAdded[removecounter]);
                    FiltersAdded.RemoveAt(removecounter);
                }
            }
        }

        /// <summary>
        /// Apply filters passed to the passed dataset
        /// </summary>
        /// <param name="dsSource">dataset to apply filters to</param>
        /// <param name="CurrentFilters">filters to use</param>
        private void RestoreFilters(DataSet dsSource, Dictionary<string, string> CurrentFilters)
        {
            foreach (string table in CurrentFilters.Keys)
            {
                dsSource.Tables[table].DefaultView.RowFilter = CurrentFilters[table];
            }
        }

        /// <summary>
        /// apply passed filters foe passed dataset and if table after applying filters have one row only will add its data to
        /// dictionary and to FiltersAdded list to remove later
        /// </summary>
        /// <param name="dsSource">dataset to apply filters to</param>
        /// <param name="filters">filters to apply</param>
        /// <param name="FiltersAdded">list to remember added data to dictionary so we can remove them later</param>
        private void WriteFilters(DataSet dsSource, Dictionary<string, List<string>> filters, List<string> FiltersAdded)
        {
            foreach (string table in filters.Keys)
            {
                string cond = "";
                foreach (string condition in filters[table])
                {
                    cond += "(" + condition + ")" + " AND ";
                }
                if (cond.EndsWith("AND "))
                    cond = cond.Remove(cond.Length - 4);
                dsSource.Tables[table].DefaultView.RowFilter = cond;

                if (dsSource.Tables[table].DefaultView.Count == 1)
                {
                    foreach (DataColumn col in dsSource.Tables[table].Columns)
                    {
                        string dicEntry = (table + "." + col.ColumnName).ToUpper();
                        if (!RowDataDictionary.Keys.Contains(dicEntry))
                        {
                            RowDataDictionary.Add(dicEntry, dsSource.Tables[table].DefaultView[0][col.ColumnName]);
                            FiltersAdded.Add(dicEntry);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Clear all filters from the passed dataset
        /// </summary>
        /// <param name="dataSet">dataset to clear filters from</param>
        private void ClearFilter(DataSet dataSet)
        {
            foreach (DataTable table in dataSet.Tables)
            {
                table.DefaultView.RowFilter = "";
            }
        }

        /// <summary>
        /// Returns dictionary contains every table name in the dataset and it's filter
        /// </summary>
        /// <param name="dsSource">dataset to extract filters from</param>
        /// <returns></returns>
        private Dictionary<string, string> GetCurrentFiltersState(DataSet dsSource)
        {
            Dictionary<string, string> filtersDictionary = new Dictionary<string, string>();
            foreach (DataTable table in dsSource.Tables)
            {
                filtersDictionary.Add(table.TableName.ToUpper(), table.DefaultView.RowFilter);
            }
            return filtersDictionary;
        }

        /// <summary>
        /// Returns list of datarow from child datatable related to passed row
        /// </summary>
        /// <param name="topRelationRow">the parent row</param>
        /// <param name="ParentToChildRelations">list of relations</param>
        /// <returns></returns>
        private List<DataRow> GetChildData(List<DataRow> topRelationRow, List<DataRelation> ParentToChildRelations)
        {
            foreach (DataRelation relation in ParentToChildRelations)
            {
                topRelationRow = GetRelationData(topRelationRow, relation);
            }
            return topRelationRow;
        }

        /// <summary>
        /// returns list of datarows which are child of rows passed and the passed relation
        /// </summary>
        /// <param name="RowsList">rows to get child rows of them</param>
        /// <param name="relation">relation used to get child rows</param>
        /// <returns></returns>
        private List<DataRow> GetRelationData(List<DataRow> RowsList, DataRelation relation)
        {
            List<DataRow> AllDataTable = new List<DataRow>();
            foreach (DataRow row in RowsList)
            {
                string condition = "";
                foreach (DataColumn column in relation.ParentColumns)
                    condition += "(" + column.Caption + " = '" + row[column] + "'" + ")" + " AND ";
                if (condition.EndsWith("AND "))
                    condition = condition.Remove(condition.Length - 4);
                string oldcondition = relation.ChildTable.DefaultView.RowFilter;
                if (oldcondition.Trim() != "")
                    condition = " ( " + oldcondition + " ) " + " AND " + "(" + condition + ")";
                relation.ChildTable.DefaultView.RowFilter = condition;

                foreach (DataRowView rowview in relation.ChildTable.DefaultView)
                    AllDataTable.Add(rowview.Row);

                relation.ChildTable.DefaultView.RowFilter = oldcondition;
            }
            return AllDataTable;
        }

        /// <summary>
        /// search for relation between the searchedtable and the relations collection passed , if relation found it will be added to the out relations.
        /// </summary>
        /// <param name="relations">relations to search in them</param>
        /// <param name="searchedTable"> table to search for it in the relations</param>
        /// <param name="outRelations"> the relation collection to add relation to </param>
        /// <returns></returns>
        private bool isRelationExist(DataRelationCollection relations, string searchedTable, ref List<DataRelation> outRelations)
        {
            foreach (DataRelation relation in relations)
            {
                if (relation.ParentTable.TableName.ToUpper() == searchedTable.ToUpper())
                {
                    outRelations.Add(relation);
                    return true;
                }
                else
                    if (isRelationExist(relation.ParentTable.ParentRelations, searchedTable, ref outRelations))
                    {
                        outRelations.Add(relation);
                        return true;
                    }
                    else
                        return false;
            }
            return false;
        }

        /// <summary>
        /// adds new row to the table based on the data saved in data dictionary
        /// </summary>
        /// <param name="table">table to add row to</param>
        private void NewRowFromDictionary(DataTable table)
        {
            XmlNodeList Mapnodes = xmlMapdoc.SelectNodes("//*/Map[" + MapXML.DestTable + "='" + table.TableName + "']");
            DataRow row = table.NewRow();
            foreach (XmlNode node in Mapnodes)
            {
                string SrcTable = node[MapXML.SrcTable].InnerText, srcField = node[MapXML.SrcField].InnerText, DestField = node[MapXML.DestField].InnerText;
                string dicEntry = (SrcTable + "." + srcField).ToUpper();
                bool Mandatory = node[MapXML.Usage] != null && node[MapXML.Usage].InnerText == "M";
                if (SrcTable != "" && srcField != "" && DestField != "")
                    row[DestField] = RowDataDictionary[dicEntry];
                else if (node[MapXML.DestField].InnerText != "" && node["Fixed"].InnerText != "")
                    row[DestField] = node["Fixed"].InnerText;
                if (Mandatory && row[DestField].ToString().Trim() == "")
                {
                    if (node[MapXML.Usage].Attributes["Msg"] != null)
                        ErrorMsgArr.Add(node[MapXML.Usage].Attributes["Msg"].Value);
                    else
                        ErrorMsgArr.Add(dicEntry + " is mandatory !!");
                }
            }
            table.Rows.Add(row);
        }

        /// <summary>
        /// Convert passed dataset to SQL tables and saved them in SQL Server Database
        /// </summary>
        /// <param name="dataset">dataset to convert</param>
        private void InsertDatasetToSQL(DataSet dataset)
        {
            if (MainDestTable != null && MainDestTable.Trim() != "")
            {
                foreach (DataRow row in dataset.Tables[MainDestTable].Rows)
                {
                    if (isPrimaryKeyExist(row))
                        DeleteFromDataSet(dataset, GetPrimaryKeyWhere(row));
                    //throw new Exception("Primary Key Exists!");
                }
            }
            foreach (DataTable table in dataset.Tables)
            {
                if (!IsTableExist(table.TableName + SqlTablesSuffix))
                    CreateTable(table);
            }
            DataTable IdentityColumnsTable = GetIdentityColumns();
            foreach (DataTable table in dataset.Tables)
            {
                DataRow[] identityRow = IdentityColumnsTable.Select("TABLE_NAME='" + table.TableName + "'");
                if (identityRow.Length > 0)
                {
                    string columnName = identityRow[0]["COLUMN_NAME"].ToString();
                    if (table.Columns[columnName] != null)
                    {
                        table.Columns[columnName].ExtendedProperties.Add("IsIdentity", true);
                    }
                }
                foreach (DataRow row in table.Select(MainSourceWhere))
                {
                    string insertcmd = GetInsertcommand(row);
                    if (!ExecuteSqlStatment(insertcmd))
                    {
                        ErrorMsg += "Failed :" + insertcmd + "\n";
                    }
                }
            }
        }

        /// <summary>
        /// Validate XML according to Specific Schemas
        /// </summary>
        /// <param name="XmlPath">XML to vaildate Path</param>
        /// <param name="schemasPath">Schemas to validate XML according to</param>
        /// <param name="ValidateErrorMsg">error msg if xml is not valid</param>
        /// <returns>True if valid otherwise false</returns>
        public bool VaildateXML(string XmlPath, out string ValidateErrorMsg, string[] schemasPath)
        {
            ValidateErrorMsg = "";
            try
            {
                System.Xml.XmlDataDocument xmlDataDocument = new XmlDataDocument();

                foreach (string schemalocation in schemasPath)
                {
                    System.Xml.Schema.XmlSchema schema = System.Xml.Schema.XmlSchema.Read(File.Open(schemalocation, FileMode.Open), null);
                    xmlDataDocument.Schemas.Add(schema);
                }
                xmlDataDocument.Schemas.Compile();
                xmlDataDocument.LoadXml(File.ReadAllText(XmlPath));
                xmlDataDocument.Validate(null);
            }
            catch (Exception ex)
            {
                ValidateErrorMsg = ex.Message;
                return false;
            }
            return true;
        }

        #region Sql Helper Methods

        /// <summary>
        /// Create and Execute SQL create table statments
        /// </summary>
        /// <param name="table">table to create</param>
        private void CreateTable(DataTable table)
        {
            SqlCommand cmd = connection.CreateCommand();
            cmd.CommandText = SqlTableCreator.GetCreateFromDataTableSQL(table.TableName + SqlTablesSuffix, table);
            cmd.Connection.Open();
            cmd.ExecuteNonQuery();
            cmd.Connection.Close();
        }

        /// <summary>
        /// Check if table already exist in SQL Server
        /// </summary>
        /// <param name="table">table to check</param>
        /// <returns>true if table exist otherwise false</returns>
        private bool IsTableExist(string table)
        {
            SqlCommand cmd = connection.CreateCommand();
            cmd.CommandText = "SELECT count(*)  FROM INFORMATION_SCHEMA.TABLES WHERE   TABLE_NAME = '" + table + "'";
            cmd.Connection.Open();
            int x = Convert.ToInt32(cmd.ExecuteScalar());
            cmd.Connection.Close();
            return x > 0;

        }

        /// <summary>
        /// Generates SQL insert command for passed row
        /// </summary>
        /// <param name="row">row to generate sql insert from</param>
        /// <returns> the generated SQL insert statment</returns>
        private string GetInsertcommand(DataRow row)
        {
            try
            {
                string Fields = string.Empty, Values = string.Empty;
                foreach (DataColumn column in row.Table.Columns)
                {
                    if (column.ExtendedProperties["IsIdentity"] != null) continue;
                    string Name = column.ToString();
                    object Value = row[Name];
                    Values += GetColumnValue(row, Name);
                    if (Value.GetType() != typeof(DBNull))
                    {
                        Fields += "[" + Name + "],";
                        Values += ",";
                    }
                }
                Values = Values.Length > 0 ? Values.Remove(Values.Length - 1) : Values;
                Fields = Fields.Length > 0 ? Fields.Remove(Fields.Length - 1) : Fields;
                string sql = "Insert into " + row.Table.TableName + SqlTablesSuffix + "(" + Fields + ") values(" + Values + ")";
                return sql;
            }
            catch (Exception ex)
            {
                throw new Exception("Error in GetInsertcommand:" + ex.Message);
            }
        }

        /// <summary>
        /// Escape any single quote chracter in the passed SQL statment
        /// </summary>
        /// <param name="Sql">SQL statment to Escape</param>
        /// <returns>the escaped SQL statment</returns>
        public static string Escape(string Sql)
        {
            string escaped = Sql.Replace("'", "''");
            return escaped;
        }

        /// <summary>
        /// Convert the passed Datetime to SQL date accepted format
        /// </summary>
        /// <param name="date">date to convert to sql</param>
        /// <returns>the converted date string</returns>
        public static string ConvertToSQLFormat(DateTime date)
        {
            return "'" + date.ToString("MM/dd/yyyy") + "'";
        }

        /// <summary>
        /// executes the passed SQL statment in SQL server
        /// </summary>
        /// <param name="sqlInsert">the SQL statment to execute</param>
        /// <returns>true if executed successfully otherwise returns false</returns>
        public bool ExecuteSqlStatment(string sqlInsert)
        {
            SqlCommand cmd = connection.CreateCommand();
            int x = 0;
            cmd.CommandText = sqlInsert;
            if (connection.State == ConnectionState.Closed || connection.State == ConnectionState.Broken)
                connection.Open();
            try
            {
                x = cmd.ExecuteNonQuery();
            }
            catch (Exception ex)
            {
                throw new Exception(ex.Message + " For command : " + cmd.CommandText);
            }
            if (connection.State == ConnectionState.Open)
                connection.Close();
            return x > 0;
        }

        /// <summary>
        /// returns Primary Key columns name for the passed table name from SQL server meta data
        /// </summary>
        /// <param name="table">table name to return Primary keys for</param>
        /// <returns>string array of found coulmns names</returns>
        public string[] GetPrimaryKey(string table)
        {
            SqlCommand cmd = connection.CreateCommand();
            cmd.CommandText = "SELECT u.COLUMN_NAME";
            cmd.CommandText += " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS c INNER JOIN";
            cmd.CommandText += " INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS u ON c.CONSTRAINT_NAME = u.CONSTRAINT_NAME";
            cmd.CommandText += " where c.CONSTRAINT_TYPE = 'PRIMARY KEY' and ";
            cmd.CommandText += " u.TABLE_NAME = '" + table + "' AND c.TABLE_NAME = '" + table + "'";
            SqlDataAdapter da = new SqlDataAdapter(cmd);
            DataTable x = new DataTable();
            da.Fill(x);
            string[] columns = new string[x.Rows.Count];
            for (int rowIndex = 0; rowIndex < x.Rows.Count; rowIndex++)
            {
                columns[rowIndex] = x.Rows[rowIndex][0].ToString();
            }
            return columns;
        }

        /// <summary>
        /// Get Datatable Contained all Identity columns in the connection DB
        /// </summary>
        /// <returns></returns>
        public DataTable GetIdentityColumns()
        {
            SqlCommand cmd = connection.CreateCommand();
            cmd.CommandText = "SELECT  t.TABLE_NAME,c.COLUMN_NAME,c.TABLE_CATALOG,c.TABLE_SCHEMA ";
            cmd.CommandText += " FROM INFORMATION_SCHEMA.COLUMNS AS c JOIN INFORMATION_SCHEMA.TABLES AS t ";
            cmd.CommandText += " ON t.TABLE_NAME = c.TABLE_NAME ";
            cmd.CommandText += "WHERE COLUMNPROPERTY(OBJECT_ID(c.TABLE_NAME),c.COLUMN_NAME,'IsIdentity') = 1 AND ";
            cmd.CommandText += "t.TABLE_TYPE = 'Base Table' AND t.TABLE_NAME NOT LIKE 'dt%' AND ";
            cmd.CommandText += "t.TABLE_NAME NOT LIKE 'MS%' AND t.TABLE_NAME NOT LIKE 'syncobj_%'";
            SqlDataAdapter da = new SqlDataAdapter(cmd);
            DataTable datatable = new DataTable();
            da.Fill(datatable);
            return datatable;
        }

        /// <summary>
        /// Checks if the passed row primary key data exists in the SQL server table
        /// </summary>
        /// <param name="row">row to check</param>
        /// <returns>true if exists otherwise false</returns>
        public bool isPrimaryKeyExist(DataRow row)
        {
            string[] pk = GetPrimaryKey(row.Table.TableName);
            if (pk.Length == 0)
                return false;
            string PKwhere = GetPrimaryKeyWhere(row);
            SqlCommand cmd = connection.CreateCommand();
            cmd.CommandText = "Select * from " + row.Table.TableName;
            cmd.CommandText += " where " + PKwhere;
            SqlDataAdapter da = new SqlDataAdapter(cmd);
            DataTable x = new DataTable();
            da.Fill(x);
            PrimaryKeyExist = x.Rows.Count > 0;
            return x.Rows.Count > 0;
        }

        /// <summary>
        /// Deletes rows matchs the where from the dataset
        /// </summary>
        /// <param name="Target">dataset to delete from</param>
        /// <param name="Where">where to execute on dataset</param>
        public void DeleteFromDataSet(DataSet Target, string Where)
        {
            SqlCommand cmd = connection.CreateCommand();
            try
            {
                cmd.Connection.Open();
                string s = "Delete from {T} where ";
                cmd.CommandText = s;
                foreach (DataTable table in Target.Tables)
                {
                    cmd.CommandText = s.Replace("{T}", table.TableName) + Where;
                    cmd.ExecuteNonQuery();
                }
                cmd.Connection.Close();
            }
            catch (Exception ex) { throw new Exception(ex.Message + " for " + cmd.CommandText); }
        }

        /// <summary>
        /// returns SQL where conditions for the primary key data in the passed row
        /// </summary>
        /// <param name="row">row to extract data from </param>
        /// <returns>the generated sql where statment</returns>
        private string GetPrimaryKeyWhere(DataRow row)
        {
            string[] pk = GetPrimaryKey(row.Table.TableName);
            string PKwhere = "";
            foreach (string pkcolumn in pk)
            {
                PKwhere += pkcolumn + " = " + GetColumnValue(row, pkcolumn);
                PKwhere += " And ";
            }
            PKwhere = PKwhere.EndsWith(" And ") ? PKwhere.Remove(PKwhere.LastIndexOf(" And ")) : PKwhere;
            return PKwhere;
        }

        /// <summary>
        /// returns value of passed column name in the passed row in SQL accepted format
        /// </summary>
        /// <param name="row">row to get value from</param>
        /// <param name="column">column name to get its value</param>
        /// <returns>the value of the passed column</returns>
        private string GetColumnValue(DataRow row, string column)
        {
            string Name = column.ToString();
            object Value = row[Name];
            string Values = "";
            if (Value.GetType() == typeof(string) || Value.GetType() == typeof(char))
            {
                Values += "'" + Escape(Value.ToString()) + "'";
            }
            else if (Value.GetType() == typeof(int) || Value.GetType() == typeof(UInt64) ||
                Value.GetType() == typeof(UInt32) || Value.GetType() == typeof(UInt16) ||
                Value.GetType() == typeof(Single) || Value.GetType() == typeof(Int64) ||
                Value.GetType() == typeof(Int32) || Value.GetType() == typeof(Int16) ||
                  Value.GetType() == typeof(decimal) || Value.GetType() == typeof(double) || Value.GetType() == typeof(Byte))
            {
                Values += Value;
            }
            else if (Value.GetType() == typeof(DateTime))
            {
                Values += ConvertToSQLFormat((DateTime)Value);
            }
            else if (Value.GetType() == typeof(bool))
            {
                Values += (bool)Value ? 1 : 0;
            }
            return Values;
        }

        /// <summary>
        /// Returns SQLconnection for the passed sql information
        /// </summary>
        /// <param name="SqlServer"></param>
        /// <param name="DataBase"></param>
        /// <param name="UserName"></param>
        /// <param name="Password"></param>
        /// <returns></returns>
        public SqlConnection GetSqlConnection(string SqlServer, string DataBase, string UserName, string Password)
        {
            SqlConnection conn = new SqlConnection();
            conn.ConnectionString = "Data Source=" + SqlServer + ";Initial Catalog=" + DataBase + ";";
            if (UserName != "")
                conn.ConnectionString += "User Id=" + UserName + ";Password=" + Password + ";";
            else
                conn.ConnectionString += "Trusted_Connection=True";

            return conn;
        }

        #endregion

        #region Properties

        public Boolean Error { get; set; }

        public string ErrorMsg { get; set; }
        private ArrayList _ErrorMsgArr;
        public ArrayList ErrorMsgArr
        {
            get
            {
                if (_ErrorMsgArr == null)
                    _ErrorMsgArr = new ArrayList();
                return _ErrorMsgArr;
            }
            set { _ErrorMsgArr = value; }
        }

        public bool PrimaryKeyExist { get; set; }

        #endregion
    }
}

//private void SetRelation(DataTable SrcTable)
//   {
//       try
//       {
//           List<DataTable> x = new List<DataTable>();
//           foreach (DataRelation relation in SrcTable.ChildRelations)
//           {
//               string filter = "";
//               foreach (DataRowView row in SrcTable.DefaultView)
//               {
//                   if (relation.ChildTable.Rows.Count > 0 && relation.ChildTable.Rows[0][relation.ChildColumns[0]].ToString() != "")
//                       filter += relation.ChildColumns[0].Caption + "='" + row[relation.ParentColumns[0].Caption] + "' OR ";
//               }
//               if (filter.EndsWith("OR "))
//                   filter = filter.Remove(filter.Length - 3);
//               foreach (XmlNode cond in xmlMapdoc.SelectNodes("//*/Map[SrcTable='" + relation.ChildTable.TableName + "']"))
//               {
//                   if (cond["Condition"].InnerText != "")
//                   {
//                       if (filter != "")
//                       {
//                           filter = "(" + filter + ")";
//                           filter = filter + " and ";
//                       }
//                       filter += cond["Condition"]["Field"].InnerText + cond["Condition"]["Value"].InnerText;
//                   }
//               }
//               string oldfilter = relation.ChildTable.DefaultView.RowFilter;
//               if (filter != "")
//               {
//                   if (oldfilter != "")
//                       oldfilter = "(" + oldfilter + ") and ";
//                   oldfilter += filter;
//               }
//               relation.ChildTable.DefaultView.RowFilter = oldfilter;

//               x.Add(relation.ChildTable);

//           }
//           foreach (DataTable dt in x)
//               SetRelation(dt);
//       }
//       catch (Exception ex)
//       {
//           throw new Exception("Error in SetRelation:" + ex.Message);
//       }
//   }

//private void FillTableData()
//     {
//         try
//         {
//             foreach (DataRow mainrow in dsSource.Tables[MainSrcTable].Rows)
//             {
//                 ClearFilter(dsSource);
//                 dsSource.Tables[MainSrcTable].DefaultView.RowFilter = dsSource.Tables[MainSrcTable].PrimaryKey[0].Caption + " = '" + mainrow[dsSource.Tables[MainSrcTable].PrimaryKey[0].Caption] + "'";
//                 //                    if (where != null && where != "")
//                 //dsSource.Tables[MainSrcTable].DefaultView.RowFilter += " And " + where;
//                 if (dsSource.Tables[MainSrcTable].DefaultView.Count > 0)
//                 {
//                     SetRelation(mainrow.Table);

//                     foreach (DataTable dest in dsTarget.Tables)
//                     {
//                         int rows = 0;
//                         XmlNodeList Mapnodes = xmlMapdoc.SelectNodes("//*/Map[DestTable='" + dest.TableName + "']");
//                         foreach (XmlNode node in Mapnodes)
//                         {
//                             if (node["SrcTable"].InnerText.Trim() != "")
//                             {
//                                 string SrcTable = node["SrcTable"].InnerText;
//                                 rows = rows < dsSource.Tables[SrcTable].DefaultView.Count ? dsSource.Tables[SrcTable].DefaultView.Count : rows;
//                             }
//                         }
//                         for (int x = 0; x < rows; x++)
//                         {
//                             dest.Rows.Add(dest.NewRow());
//                             foreach (XmlNode node in Mapnodes)
//                             {
//                                 object value = null;
//                                 string SrcTable = node["SrcTable"].InnerText, srcField = node["srcField"].InnerText, DestField = node["DestField"].InnerText;
//                                 if (SrcTable != "" && srcField != "" && DestField != "")
//                                 {
//                                     if (dsSource.Tables[SrcTable].DefaultView.Count > 0)
//                                     {
//                                         if (dsSource.Tables[SrcTable].DefaultView.Count - 1 >= x)
//                                             value = dsSource.Tables[SrcTable].DefaultView[x][srcField];
//                                         else
//                                             value = dsSource.Tables[SrcTable].DefaultView[0][srcField];

//                                         dest.Rows[dest.Rows.Count - 1][DestField] = value;
//                                     }
//                                 }
//                                 else if (node["DestField"].InnerText != "" && node["Fixed"].InnerText != "")
//                                 {
//                                     if (node["Condition"].InnerText != "")
//                                     {
//                                         string condTablestr = node["Condition"]["Table"].InnerText;
//                                         string condFieldstr = node["Condition"]["Field"].InnerText;
//                                         string condValuestr = node["Condition"]["Value"].InnerText;
//                                         DataTable cond = dsSource.Tables[condTablestr].DefaultView.ToTable();
//                                         if (cond.Rows.Count > 0)
//                                         {
//                                             cond.DefaultView.RowFilter = condFieldstr + " " + condValuestr;
//                                             if (cond.DefaultView.Count > 0)
//                                                 value = node["Fixed"].InnerText;
//                                         }

//                                     }
//                                     else
//                                     {
//                                         value = node["Fixed"].InnerText;
//                                     }
//                                     dest.Rows[dest.Rows.Count - 1][DestField] = value;
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//         catch (Exception ex)
//         {
//             throw new Exception("Error in FillTableData:" + ex.Message);
//         }
//     }