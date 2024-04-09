using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.DataTypes.ObjectDictionary;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using Aria.Environment;
using Aria.Utilities.Aria40Converter.Helpers;


namespace Aria.Utilities.Aria40Converter.AriaDictionaryTree
{
    class AriaRelatedDataObjectRevision : AriaDictionaryObjectCollection
    {
        public AriaObjectRevision AriaObjectRevision = new AriaObjectRevision();

        public AriaRelatedDataObjectRevision(AriaDictionaryObject parent)
            : base(parent)
        {
            AriaObjectRevision.ObjectRevision = AriaDictionaryObject.CurrentRevision;

            AriaObjectRevision.ObjectRevisionSettings = new AriaRelatedDataObjectSettings();

            SetAriaRelatedDataObjectRevisionSettings((AriaRelatedDataObjectSettings)AriaObjectRevision.ObjectRevisionSettings,
                                                     ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject))).AriaDataObjectTable);
        }

        public void SetAriaRelatedDataObjectRevisionSettings(AriaRelatedDataObjectSettings settings, Table table)
        {
            settings.DataObjectName = AriaSchema.GetTableNewObjectName(table.TableName);

            if (((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.Filter.Trim() != "")
            {
                settings.Filter = ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.Filter;
            }
            else
            {

                string ObjectNameRelated = ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject))).AriaObject.ObjectName;
                string ObjectNameParent = ((AriaDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaDataObject))).AriaObject.ObjectName;

                List<string> columnsRelated = new List<string>();

                string filterRelated = "";

                List<string> columnsParent = new List<string>();

                ObjectNameRelated = "[" + ObjectNameRelated + "]";

                ObjectNameParent = "[" + ObjectNameParent + "]";


                if (((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TableName == "CODES")
                {
                    string tableFieldName = ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TablePrimaryKeyFields[0].Trim();
                    columnsRelated.Add(ObjectNameRelated + ".[CDEFCODE] = 'N'");
                    columnsRelated.Add(ObjectNameRelated + ".[CCODE_NO] = " + ObjectNameParent + "." + "[" + tableFieldName + "]");
                    columnsRelated.Add(ObjectNameRelated + ".[CRLTFIELD] = 'N'");
                    columnsRelated.Add(ObjectNameRelated + ".[CFLD_NAME] = '" + tableFieldName + "'");
                }
                else
                {
                    columnsRelated.AddRange(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TablePrimaryKeyFields.Select(p => p.Trim()).ToList());
                    columnsParent.AddRange(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.RelatedTablePrimaryKeyFields.Select(p => p.Trim()).ToList());
                    for (int index = 0; index < columnsRelated.Count; index++)
                    {
                        if (columnsParent[index].Trim().StartsWith("'"))
                        {
                            columnsRelated[index] = ObjectNameRelated + "." + "[" + columnsRelated[index] + "] = " + columnsParent[index];
                        }
                        else
                        {
                            columnsRelated[index] = ObjectNameRelated + "." + "[" + columnsRelated[index] + "] = " + ObjectNameParent + "." + "[" + columnsParent[index] + "]";
                        }
                    }
                }
                filterRelated = string.Join(" AND ", columnsRelated.ToArray());

                string filter = "";
                if (table.DatabaseType.Equals(DatabaseTypes.Aria27Data) || table.DatabaseType.Equals(DatabaseTypes.Aria27SystemFiles))
                {
                    List<string> leftSideFilter = new List<string>();
                    if (((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TableName == "CODES")
                    {
                        leftSideFilter.Add("CDEFCODE");
                        leftSideFilter.Add("CCODE_NO");
                        leftSideFilter.Add("CRLTFIELD");
                        leftSideFilter.Add("CFLD_NAME");
                    }
                    else
                    {
                        leftSideFilter.AddRange(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TablePrimaryKeyFields.Select(p => p.Trim()).ToList());
                    }
                    List<string> rightSideFilter = new List<string>();

                    string relatedTableName = ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TableName;
                    string relatedTableNameExpression = AriaSchema.GetTable(relatedTableName).PrimaryKey.Expression;

                    if (((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TableName == "CODES")
                    {
                        string tableFieldName = ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TablePrimaryKeyFields[0].Trim();
                        rightSideFilter.Add("'N'");
                        rightSideFilter.Add("'@" + tableFieldName + "@'");
                        rightSideFilter.Add("'N'");
                        rightSideFilter.Add("'" + tableFieldName + "'");

                        filter = relatedTableNameExpression + " = " + string.Join("+", rightSideFilter.ToArray());
                    }
                    else
                    {
                        string[] ColumnsNameOrg = table.PrimaryKey.Expression.Split('+');
                        IndexField[] ColumnsName = VFPSchemaHelper.GetIndexFields(table.PrimaryKey.Expression, SortTypes.Ascending);

                        rightSideFilter.AddRange(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.RelatedTablePrimaryKeyFields.Select(p => p.Trim()).ToList());

                        for (int i = 0; i < rightSideFilter.Count; i++)
                        {
                            if (rightSideFilter[i].Trim().StartsWith("'"))
                            {
                                ColumnsNameOrg[i] = rightSideFilter[i].Trim();
                            }
                            else
                            {
                                if (AriaSchema.GetTableFields(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.RelatedTableName).First(r => r.FieldName.Trim().ToUpper() == rightSideFilter[i].Trim().ToUpper()).DataType == FieldDataTypes.String)
                                    ColumnsNameOrg[i] = ColumnsNameOrg[i].Replace(ColumnsName[i].FieldName, "'@" + rightSideFilter[i] + "@'");
                                else if (AriaSchema.GetTableFields(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.RelatedTableName).First(r => r.FieldName.Trim().ToUpper() == rightSideFilter[i].Trim().ToUpper()).DataType == FieldDataTypes.Number)
                                    ColumnsNameOrg[i] = ColumnsNameOrg[i].Replace(ColumnsName[i].FieldName, "@" + rightSideFilter[i] + "@");
                                else if (AriaSchema.GetTableFields(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.RelatedTableName).First(r => r.FieldName.Trim().ToUpper() == rightSideFilter[i].Trim().ToUpper()).DataType == FieldDataTypes.Date)
                                    ColumnsNameOrg[i] = ColumnsNameOrg[i].Replace(ColumnsName[i].FieldName, "{@" + rightSideFilter[i] + "@}");
                            }
                        }

                        filter = relatedTableNameExpression + " = " + string.Join("+", ColumnsNameOrg);
                    }
                }
                else
                {
                    List<string> leftSideFilter = new List<string>();
                    leftSideFilter.AddRange(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.TablePrimaryKeyFields.Select(p => p.Trim()).ToList());
                    List<string> rightSideFilter = new List<string>();
                    rightSideFilter.AddRange(((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject)))._relation.RelatedTablePrimaryKeyFields.Select(p => p.Trim()).ToList());

                    for (int i = 0; i < rightSideFilter.Count; i++)
                    {
                        if (!rightSideFilter[i].Trim().StartsWith("'"))
                        {
                            rightSideFilter[i] = "[" + leftSideFilter[i] + "] = @" + rightSideFilter[i];
                        }
                    }

                    filter = string.Join("AND", rightSideFilter.ToArray());

                }

                settings.Filter = filter + " | " + filterRelated;
            }

            settings.DataObjectRevision = AriaDictionaryObject.CurrentRevision;
            settings.ModificationType = AriaModificationTypes.Add;
            settings.ParentDataObjectRevision = AriaDictionaryObject.CurrentRevision;
        }

        public override void CreateChildren()
        {
            this.Children.Add(new AriaRelatedDataObjectFields(this));
            base.CreateChildren();
        }

        public override void Save()
        {
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();

            AriaXmlSerializer xmlSerializer = new AriaXmlSerializer();
            string xmlSettings = xmlSerializer.ConvertToXml(AriaObjectRevision.ObjectRevisionSettings);

            objectDictionary.SaveAriaObjectRevision(new AriaDbConnection("", ""),
                                                     ((AriaRelatedDataObject)AriaDictionaryObject.GetParent(this, typeof(AriaRelatedDataObject))).AriaObject.ObjectID,
                                                    AriaObjectRevision.ObjectRevision,
                                                    xmlSettings,
                                                    "");

            base.Save();
        }
    }
}
