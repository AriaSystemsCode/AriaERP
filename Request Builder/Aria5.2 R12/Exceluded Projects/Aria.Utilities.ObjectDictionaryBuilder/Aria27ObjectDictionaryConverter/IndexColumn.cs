using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public class IndexColumn
    {
        public IndexColumn()
            : this("")
        {
        }

        public IndexColumn(string colmnName)
            : this(colmnName, EnumSort.NotSet)
        {
        }

        public IndexColumn(string columnName, EnumSort sort)
        {
            this.ColumnName = columnName;
            this.Sort = sort;
        }

        private string _columnName;
        public string ColumnName
        {
            get { return _columnName; }
            set { _columnName = value; }
        }

        private EnumSort _sort;
        public EnumSort Sort
        {
            get { return _sort; }
            set { _sort = value; }
        }
    }
}
