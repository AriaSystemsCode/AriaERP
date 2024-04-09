using System;
using System.Collections.Generic;
using System.Text;
using System.Data.Odbc;
using System.Data;
using System.Collections;
using System.Linq;
using System.Linq.Expressions;
using Aria.Utilities.Aria40Converter.Helpers;
using System.IO;
using System.Windows.Forms;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public enum CreatingSystemMasterTypes
    {
        NotSet,
        All,
        Merge
    }

    class Aria50SchemaInformation : Aria4XPSchemaInformation
    {
        public CreatingSystemMasterTypes CreatingSystemMasterType { get; set; }

        private DataTable _ariaObject = new DataTable();
        private DataTable _ariaObjectProperty = new DataTable();

        public Aria50SchemaInformation(string connectionString4xp, string connectionString27, string mergePath4Xp, string mergePath27)
            : base(connectionString4xp, connectionString27, mergePath4Xp, mergePath27)
        {
            string objectXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObject.XML");
            _ariaObject.ReadXml(objectXml);

            string objectPropertyXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObjectProperty.XML");
            _ariaObjectProperty.ReadXml(objectPropertyXml);

            CreatingSystemMasterType = CreatingSystemMasterTypes.All;
        }

        public void Refresh()
        {
            _ariaObject = new DataTable();
            _ariaObjectProperty = new DataTable();

            string objectXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObject.XML");
            _ariaObject.ReadXml(objectXml);

            string objectPropertyXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObjectProperty.XML");
            _ariaObjectProperty.ReadXml(objectPropertyXml);
        }

        public string GetTableNewObjectName(string tableName)
        {
            if (_ariaObject.Select("TableName = '" + tableName + "'").Length > 0)
            {
                for (int i = 0; i < _ariaObject.Select("TableName = '" + tableName + "'").Length; i++)
                {
                    string objName = _ariaObject.Select("TableName = '" + tableName + "'")[i]["NewObjectName"].ToString();

                    if (objName.Split('.').Length == 2)
                    {
                        return objName;
                    }
                }
            }

            return null;
        }

        public void AddObject(string ObjectName, string tableName)
        {
            if (_ariaObject.Select("OrgObjectName = '" + ObjectName + "'").Length == 0)
            {
                _ariaObject.Rows.Add(false, ObjectName, DBNull.Value, tableName);
            }
        }

        public bool IsOrgObjectExist(string orgObjectName)
        {
            return _ariaObject.Select("OrgObjectName = '" + orgObjectName + "' AND Select = 1").Length > 0;
        }

        public bool IsNewObjectExist(string newObjectName)
        {
            if (_ariaObject.Select("NewObjectName = '" + newObjectName + "' AND Select = 1").Length > 0)
            {
                return true;
            }
            else
            {
                if (_ariaObject.Select("OrgObjectName = '" + newObjectName + "' AND Select = 1").Length > 0)
                {
                    if (_ariaObject.Select("OrgObjectName = '" + newObjectName + "' AND Select = 1")[0]["NewObjectName"] == DBNull.Value ||
                       _ariaObject.Select("OrgObjectName = '" + newObjectName + "' AND Select = 1")[0]["NewObjectName"].ToString().Trim() == "")
                    {
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
                else
                {
                    return false;
                }
            }
        }

        public bool IsNewObjectCreateEvents(string newObjectName)
        {
            if (_ariaObject.Select("NewObjectName = '" + newObjectName + "'")[0]["CreateEvents"] == DBNull.Value)
            {
                return false;
            }
            else
            {
                return Convert.ToBoolean(_ariaObject.Select("NewObjectName = '" + newObjectName + "'")[0]["CreateEvents"]);
            }
        }

        public string GetNewObjectName(string orgObjectName)
        {
            if (_ariaObject.Select("OrgObjectName = '" + orgObjectName + "'")[0]["NewObjectName"] == DBNull.Value ||
               _ariaObject.Select("OrgObjectName = '" + orgObjectName + "'")[0]["NewObjectName"].ToString().Trim() == "")
            {
                return orgObjectName;
            }
            else
            {
                return _ariaObject.Select("OrgObjectName = '" + orgObjectName + "'")[0]["NewObjectName"].ToString();
            }
        }

        public string GetFixedFilter(string orgObjectName)
        {
            if (_ariaObject.Select("OrgObjectName = '" + orgObjectName + "'")[0]["FixedFilter"] == DBNull.Value ||
               _ariaObject.Select("OrgObjectName = '" + orgObjectName + "'")[0]["FixedFilter"].ToString().Trim() == "")
            {
                return "";
            }
            else
            {
                return _ariaObject.Select("OrgObjectName = '" + orgObjectName + "'")[0]["FixedFilter"].ToString();
            }
        }

        public void AddObjectProperty(string objectName, string orgObjectPropertyName, string fieldName)
        {
            if (_ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + orgObjectPropertyName.Replace("'", "''") + "'").Length == 0)
            {
                _ariaObjectProperty.Rows.Add(false, objectName, orgObjectPropertyName, DBNull.Value, fieldName);
            }
        }

        public bool IsOrgObjectPropertyExist(string objectName, string orgObjectPropertyName)
        {
            return _ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + orgObjectPropertyName.Replace("'", "''") + "' AND Select = 1").Length > 0;
        }

        public bool IsNewObjectPropertyExist(string objectName, string newObjectPropertyName)
        {
            if (_ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND NewObjectPropertyName = '" + newObjectPropertyName.Replace("'", "''") + "' AND Select = 1").Length > 0)
            {
                return true;
            }
            else
            {
                if (_ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + newObjectPropertyName + "' AND Select = 1").Length > 0)
                {
                    if (_ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + newObjectPropertyName + "' AND Select = 1")[0]["NewObjectPropertyName"] == DBNull.Value ||
                       _ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + newObjectPropertyName + "' AND Select = 1")[0]["NewObjectPropertyName"].ToString().Trim() == "")
                    {
                        return true;
                    }
                    else
                    {
                        return false;
                    }
                }
                else
                {
                    return false;
                }
            }
        }

        public string GetNewObjectPropertyName(string objectName, string orgObjectPropertyName)
        {
            if (_ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + orgObjectPropertyName + "'")[0]["NewObjectPropertyName"] == DBNull.Value ||
                _ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + orgObjectPropertyName + "'")[0]["NewObjectPropertyName"].ToString().Trim() == "")
            {
                return orgObjectPropertyName;
            }
            else
            {
                return _ariaObjectProperty.Select("ObjectName = '" + objectName + "' AND OrgObjectPropertyName = '" + orgObjectPropertyName + "'")[0]["NewObjectPropertyName"].ToString();
            }
        }

        public void SaveAll()
        {
            string objectXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObject.XML");
            _ariaObject.WriteXml(objectXml, XmlWriteMode.WriteSchema);
 
            string objectPropertyXml = Path.Combine(Application.StartupPath, @"MergeData\Aria5\AriaObjectProperty.XML");
            _ariaObjectProperty.WriteXml(objectPropertyXml, XmlWriteMode.WriteSchema);
        }
    }
}