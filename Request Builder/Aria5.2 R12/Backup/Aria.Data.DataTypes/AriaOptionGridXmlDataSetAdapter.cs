using Aria.DataTypes.Settings;
using Aria.DataTypes;
using System.Xml;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.DataTypes.ObjectDictionary;
using System.IO;
//[Add]Ahmed Maher Date: 29/03/2010
using Aria.HelperClass.SAAS;
using System;
//[END]

namespace Aria.Data.DataTypes
{
    public class AriaOptionGridXmlDataSetAdapter : AriaDataTypeAdapter
    {
        public AriaOptionGridXmlDataSetAdapter(AriaDbConnection connection, AriaDataTypeSettings settings, AriaDataType optionGridPointer)
            : base(connection, settings, optionGridPointer)
        {
            this.Settings = settings;
            this.Value = optionGridPointer;
        }

        private string GetDataFromXml(XmlNodeList xmlNodeList, string fieldName,string clientId)
        {
            //[Add]Ahmed Maher -Date: 29/03/2010
            AriaLogManage LogM = new AriaLogManage();
            try
            {
                //[End]
                fieldName = fieldName.ToUpper().Trim();

                for (int nodeIndex = 0; nodeIndex < xmlNodeList.Count; nodeIndex++)
                {
                    if (xmlNodeList[nodeIndex].ChildNodes[1].InnerText.Trim().ToUpper().Equals(fieldName))
                    {
                        return xmlNodeList[nodeIndex].ChildNodes[2].InnerText;
                    }
                }
                //[Add]Ahmed Maher Date: 29/03/2010
            }
            catch (Exception Ex)
            {
                //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
                //LogM.AddLog("Error", "Aria.Data.DataTypes.AriaOptionGridXmlDataSetAdapter.GetDataFromXml", Ex.Message, clientId);
                //SAB 04-09-2013 Remove Write to Event Log Test Code [End]
            }
            //[END]
            return "";
        }

        public override object GetData(string path,string clientId)
        {
            //[Add]Ahmed Maher -Date: 29/03/2010
            AriaLogManage LogM = new AriaLogManage();            
            //[End]
            AriaObjectDictionaryDBCentric objectDictionary = new AriaObjectDictionaryDBCentric();
            string objectName = ((AriaOptionGridXmlDataSetSettings)Settings).OptionGridObjectName;
            string fieldName =
                    ((AriaOptionSettings)objectDictionary.LoadAriaObjectProperty(Connection, objectName,
                      objectDictionary.LoadActiveRevision(Connection, objectName, clientId).ObjectRevision, path, clientId).PropertySettings).FieldName.ToUpper();
            fieldName = fieldName.ToUpper().Trim();
            XmlDocument xml = new XmlDocument();
            xml.Load(((AriaOptionGridXmlDataSet)Value).FileName);

            XmlNodeList xmlNodeList = xml.ChildNodes[1].ChildNodes[0].ChildNodes;

            if (fieldName.Contains("."))
            {
                //[Add]Ahmed Maher -Date: 29/03/2010
                try
                {
                //[End]
                    for (int nodeIndex = 0; nodeIndex < xmlNodeList.Count; nodeIndex++)
                    {
                        if (xmlNodeList[nodeIndex].ChildNodes[2].InnerText.Trim().ToUpper().Equals(fieldName.Trim()))
                        {
                            string valueType = GetDataFromXml(xmlNodeList,
                                                                xmlNodeList[nodeIndex].ChildNodes[1].InnerText.Replace(" ", "").Replace(",1]", ",7]"), clientId);
                            if (valueType.Trim().ToUpper() == "V")
                            {
                                return GetDataFromXml(xmlNodeList, xmlNodeList[nodeIndex].ChildNodes[1].InnerText.Replace(" ", "").Replace(",1]", ",6]"), clientId);
                            }
                            else
                            {
                                string tempCursorName = GetDataFromXml(xmlNodeList,
                                                                        xmlNodeList[nodeIndex].ChildNodes[1].InnerText.Replace(" ", "").Replace(",1]", ",6]"), clientId);
                                string tempCursorValue = GetDataFromXml(xmlNodeList, tempCursorName, clientId);

                                XmlDocument innerXml = new XmlDocument();
                                innerXml.LoadXml(tempCursorValue);

                                XmlNodeList xmlInnerNodeList = innerXml.ChildNodes[1].ChildNodes[0].ChildNodes;
                                string returnValue = "";

                                for (int innerNodeIndex = 0; innerNodeIndex < xmlInnerNodeList.Count; innerNodeIndex++)
                                {
                                    if (innerNodeIndex != 0)
                                    {
                                        returnValue += ", ";
                                    }

                                    returnValue += xmlInnerNodeList[innerNodeIndex].ChildNodes[0].InnerText;
                                }
                                return returnValue;
                            }
                        }
                    }
                    //[Add]Ahmed Maher -Date: 29/03/2010
                }
                catch (Exception Ex)
                {
                    //SAB 04-09-2013 Remove Write to Event Log Test Code [Start]
                    //LogM.AddLog("Error", "Aria.Data.DataTypes.AriaOptionGridXmlDataSetAdapter.GetData", Ex.Message, clientId);
                    //SAB 04-09-2013 Remove Write to Event Log Test Code [End]
                }
                //[END]
                return "";
            }
            else
            {
                return GetDataFromXml(xmlNodeList, fieldName, clientId);
            }

        }

        public override void SetData(string dataPath, object value)
        {
            throw new System.Exception("The method or operation is not implemented.");
        }
    }
}