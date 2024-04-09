using System.Data;

/// <summary>
/// Helper methods and functions.
/// </summary>
/// <typeparam name="T">A strongly type DataTable.
/// A DataTable of type T will be returned from the DataSet.
/// </typeparam>
public static class TypedDataConvertor<T> where T : DataTable, new()
{
    /// <summary>
    /// Convert the first DataTable from a DataSet to a
    /// strongly-typed data table.
    /// </summary>
    public static T convert(DataSet dataSet)
    {
        if (dataSet == null)
            return null;
        if (dataSet.Tables.Count == 0)
            return null;
        DataTable dataTable = dataSet.Tables[0];
        return convert(dataTable);
    }
    /// <summary>
    /// Convert an ordinary DataTable to a strongly-typed
    /// data table.
    /// </summary>
    public static T convert(DataTable dataTable)
    {
        if (dataTable == null)
            return null;
        
        using (T stronglyTyped = new T())
        {
            stronglyTyped.Merge(dataTable, true, MissingSchemaAction.Ignore);
            dataTable.Dispose();
            return stronglyTyped;
        }
    }
}
