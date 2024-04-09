using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class TableRelation
    {
        private string _tableName;
        public string TableName
        {
            get { return _tableName; }
            set { _tableName = value; }
        }

        private string _relatedTableName;
        public string RelatedTableName
        {
            get { return _relatedTableName; }
            set { _relatedTableName = value; }
        }

        private RelationTypes _relationType;
        public RelationTypes RelationType
        {
            get { return _relationType; }
            set { _relationType = value; }
        }

        private string _relationName;
        public string RelationName
        {
            get { return _relationName; }
            set { _relationName = value; }
        }

        private string _relationTitle;
        public string RelationTitle
        {
            get { return _relationTitle; }
            set { _relationTitle = value; }
        }

        private string _filter = "";
        public string Filter
        {
            get { return _filter; }
            set { _filter = value; }
        }

        private List<string> _tablePrimaryKeyFields = new List<string>();
        public List<string> TablePrimaryKeyFields
        {
            get { return _tablePrimaryKeyFields; }
            set { _tablePrimaryKeyFields = value; }
        }

        private List<string> _relatedTablePrimaryKeyFields = new List<string>();
        public List<string> RelatedTablePrimaryKeyFields
        {
            get { return _relatedTablePrimaryKeyFields; }
            set { _relatedTablePrimaryKeyFields = value; }
        }
    }
}
