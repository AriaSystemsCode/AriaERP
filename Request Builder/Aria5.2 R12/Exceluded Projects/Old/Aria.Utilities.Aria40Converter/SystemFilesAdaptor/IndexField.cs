using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class IndexField
    {
        public IndexField()
            : this("")
        {
        }

        public IndexField(string colmnName)
            : this(colmnName, SortTypes.NotSet)
        {
        }

        public IndexField(string columnName, SortTypes sort)
        {
            this.FieldName = columnName;
            this.Sort = sort;
        }

        private string _fieldName;
        public string FieldName
        {
            get { return _fieldName; }
            set { _fieldName = value; }
        }

        private SortTypes _sort;
        public SortTypes Sort
        {
            get { return _sort; }
            set { _sort = value; }
        }
    }
}
