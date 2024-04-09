using System;
using System.Collections.Generic;
using System.Text;
using System.IO;


namespace AriaObjectBrowser.DataTypes
{
    public delegate void AriaObjectChangedEventHandler();
    
    public class AriaObject
    {
        public event AriaObjectChangedEventHandler Changed;

        private string _objectId;
        public string ObjectId
        {
            get { return _objectId; }
            set { _objectId = value; if (Changed != null) Changed(); }
        }

        private string _name;
        public string Name
        {
            get { return _name; }
            set { _name = value; if (Changed != null) Changed(); }
        }

        private string _notes;
        public string Notes
        {
            get { return _notes; }
            set { _notes = value; if (Changed != null) Changed(); }
        }

        private FileInfo _file;
        public FileInfo File
        {
            get { return _file; }
            set { _file = value; if (Changed != null) Changed(); }
        }

        private bool _isDefault;
        public bool IsDefault
        {
            get { return _isDefault; }
            set { _isDefault = value; if (Changed != null) Changed(); }
        }

        private bool _isActive;
        public bool IsActive
        {
            get { return _isActive; }
            set { _isActive = value; }
        }

        private bool _isPicture;
        public bool IsPicture
        {
            get { return _isPicture; }
            set { _isPicture = value; if (Changed != null) Changed(); }
        }

        private string _sftFormatFileName;
        public string SftFormatFileName
        {
            get { return _sftFormatFileName; }
            set { _sftFormatFileName = value; }
        }
    }
}
