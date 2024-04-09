using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.ObjectDictionaryBuilder.Aria27ObjectDictionaryConverter
{
    public enum EnumSort
    {
        NotSet,
        Ascending,
        Descending
    }

    public enum DatabaseType
    {
        Aria27Data,
        Aria4Data
    }

    public enum EnumDataType
    {
        NotSet,
        String,
        Number,
        Date,
        Logical,
        Memo,
        General
    }

    public enum EnumIndexType
    {
        NotSet,
        PrimaryKey,
        Index
    }

    public enum EnumUpgradeLevel
    {
        Standard,
        Custom
    }
}
