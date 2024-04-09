using System;

namespace Aria.DataTypes.ObjectDictionary
{
    public enum AriaObjectTypes
    {   
        Package,
        Data,
        RelatedData,
        Presenatation,
        Server,
        OptionGrid,
        Report,
        Utility,
        Framework
    }

    public enum AriaModificationTypes
    {
        Add,
        Modify,
        Delete
    }

    public enum AriaOutputFormatTypes
    {
        Pdf,
        Xml,
        Excel,
        Txt,
        Html
    }
}
