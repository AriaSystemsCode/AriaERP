using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class OptionGrid : IEqualityComparer<OptionGrid>
    {
        public OptionGrid()
        {
        }

        private string _reportName;
        public string ReportName
        {
            get { return _reportName; }
            set { _reportName = value; }
        }

        private string _reportID;
        public string ReportID
        {
            get { return _reportID; }
            set { _reportID = value; }
        }

        private OptionGridTypes _type;
        public OptionGridTypes Type
        {
            get { return _type; }
            set { _type = value; }
        }

        private UpgradeLevelTypes _upgradeLevel;
        public UpgradeLevelTypes UpgradeLevel
        {
            get { return _upgradeLevel; }
            set { _upgradeLevel = value; }
        }

        #region IEqualityComparer<OptionGrid> Members

        public bool Equals(OptionGrid x, OptionGrid y)
        {
            return x.ReportName == y.ReportName;//COMPARE REPORT ID
        }

        public int GetHashCode(OptionGrid obj)
        {
            return obj.GetHashCode();
        }

        #endregion
    }
}
