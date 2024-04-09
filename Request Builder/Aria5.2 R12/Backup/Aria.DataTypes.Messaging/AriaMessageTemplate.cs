using System;
using Aria.Xml;
using System.ComponentModel;

namespace Aria.DataTypes.Messaging
{
    /// <summary>
    /// Create properties can be used in Email Template.
    /// </summary>
    [Serializable, AriaSerializableAttribute, TypeConverter(typeof(ExpandableObjectConverter))]
    public class AriaMessageTemplate
    {
        private string _templateID = "";
        public string TemplateID
        {
            get { return _templateID; }
            set { _templateID = value; }
        }

        private string _objectName = "";
        public string ObjectName
        {
            get { return _objectName; }
            set { _objectName = value; }
        }

        private string _description = "";
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        private string _to = "";
        public string To
        {
            get { return _to; }
            set { _to = value; }
        }

        private string _cc = "";
        public string Cc
        {
            get { return _cc; }
            set { _cc = value; }
        }

        private string _bcc = "";
        public string Bcc
        {
            get { return _bcc; }
            set { _bcc = value; }
        }

        private string _subject = "";
        public string Subject
        {
            get { return _subject; }
            set { _subject = value; }
        }

        private string _body = "";
        public string Body
        {
            get { return _body; }
            set { _body = value; }
        }

        private bool _default = false;
        public bool Default
        {
            get { return _default; }
            set { _default = value; }
        }
    }
}
