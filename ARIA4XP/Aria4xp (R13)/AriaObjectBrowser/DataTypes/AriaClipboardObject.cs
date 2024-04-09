using System;
using System.Collections.Generic;
using System.Text;
using System.IO;


namespace AriaObjectBrowser.DataTypes
{
    public class AriaClipboardObject
    {
        private string _objectId;
        public string ObjectId
        {
            get { return _objectId; }
            set { _objectId = value; }
        }

        private string _name;
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        private string _notes;
        public string Notes
        {
            get { return _notes; }
            set { _notes = value; }
        }

        private string _file;
        public string File
        {
            get { return _file; }
            set { _file = value; }
        }

        private bool _isDefault;
        public bool IsDefault
        {
            get { return _isDefault; }
            set { _isDefault = value;}
        }
    }
}
