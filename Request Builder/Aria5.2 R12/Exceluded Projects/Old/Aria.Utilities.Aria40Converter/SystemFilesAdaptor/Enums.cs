using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public enum SortTypes
    {
        NotSet,
        Ascending,
        Descending
    }

    public enum DatabaseTypes
    {
        NotSet,
        Aria27SystemFiles,
        Aria27Data,
        Aria4Data
    }

    public enum FieldDataTypes
    {
        NotSet,
        String,
        Number,
        Date,
        Logical,
        Memo,
        General
    }

    public enum IndexTypes
    {
        NotSet,
        PrimaryKey,
        Index
    }

    public enum OptionGridTypes

    {
        NotSet,
        Report,
        Program
    }

    public enum OptionGridVariableValueTypes
    {
        NotSet,
        Fixed,
        Expression
    }

    public enum UpgradeLevelTypes
    {
        NotSet,
        Standard,
        Custom
    }

    public enum RelationTypes
    {
        NotSet,
        ParentChild,
        Related
    }
}
