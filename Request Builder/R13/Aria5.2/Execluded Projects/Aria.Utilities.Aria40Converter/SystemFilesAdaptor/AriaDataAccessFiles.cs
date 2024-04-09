using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Aria.DataTypes.ObjectDictionary.Settings.Object;
using Aria.Utilities.Aria40Converter;
using Aria.Environment;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using Aria.Data;
using Aria.Xml;
using System.Text.RegularExpressions;
using System.Data;
using Aria.DataTypes.Settings;
using Aria.DataTypes;
using Aria.Utilities.Aria40Converter.SystemFilesAdaptor;
using System.Data.Odbc;
using System.Xml;
using System.Windows.Forms;
using System.IO;


namespace Aria.Utilities.Aria40Converter.SystemFilesAdaptor
{
    public class AriaDataAccessFiles : SchemaDataAccess
    {
        public AriaDataAccessFiles(string connectionString, string mergePath)
            : base(connectionString, mergePath)
        {
        }

        public AriaDataAccessFiles(string connectionString)
            : base(connectionString)
        {
        }

        public override DataTable GetTableDataTable()
        {
            OdbcCommand tableCommand = new OdbcCommand();
            tableCommand.CommandText = "SELECT DISTINCT CFILE_NAM, CFILE_TTL, CUPGRDLVL, LSQLFILE, CFILE_TAG FROM SYDFILES";
            tableCommand.Connection = new OdbcConnection(this.ConnectionString);
            tableCommand.Connection.Open();

            OdbcDataAdapter tableAdapter = new OdbcDataAdapter(tableCommand);
            DataTable table = new DataTable();

            tableAdapter.Fill(table);
            tableCommand.Connection.Close();

            table.TableName = "SYDFILES";
            string[] pk = new string[1];
            pk[0] = "CFILE_NAM";

            if (!File.Exists(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFILES.XML")))
            {
                DataTable tempTable = table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFILES.XML"), XmlWriteMode.WriteSchema);
            }

            Helpers.MergeTables(table, Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFILES.XML"), pk);

            return table;
        }

        public override DataTable GetTableIndexDataTable()
        {
            OdbcCommand command = new OdbcCommand();
            OdbcDataAdapter adapter;
            DataTable table = new DataTable();
            command.CommandText = "SELECT DISTINCT CFILE_NAM, MINDX_DES, CFILE_TAG, CUPGRDLVL, CINDX_EXP, LASCEND FROM SYDINDEX";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();
            adapter = new OdbcDataAdapter(command);
            adapter.Fill(table);
            command.Connection.Close();

            table.TableName = "SYDINDEX";
            string[] pk = new string[2];
            pk[0] = "CFILE_NAM";
            pk[1] = "CFILE_TAG";

            if (!File.Exists(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDINDEX.XML")))
            {
                DataTable tempTable = table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDINDEX.XML"), XmlWriteMode.WriteSchema);
            }

            Helpers.MergeTables(table, Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDINDEX.XML"), pk);

            return table;
        }

        public override DataTable GetFieldDataTable()
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT DISTINCT CFLD_NAME,CDATA_TYP,NFLD_WDTH,NFLD_DEC,CFLD_HEAD,MVALD_STR,CPICT_STR,CFLD_MSG,MFLD_DES,LVLDENTRY,LRLTFIELDS,MRLTFIELDS,MVENTRIES,CUPGRDLVL,CVLDFNLOC,LRELATED,MCODEINFO,LVLDENTRY FROM SYDFIELD";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            table.TableName = "SYDFIELD";
            string[] pk = new string[1];
            pk[0] = "cfld_name";

            if (!File.Exists(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFIELD.XML")))
            {
                DataTable tempTable = table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFIELD.XML"), XmlWriteMode.WriteSchema);
            }

            Helpers.MergeTables(table, Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFIELD.XML"), pk);

            return table;
        }

        public override DataTable GetTableFieldDataTable()
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT DISTINCT SYDFLFLD.CFILE_NAM,SYDFLFLD.CFLD_NAME, SYDFIELD.CFLD_HEAD, SYDFLFLD.CUPGRDLVL FROM SYDFLFLD LEFT JOIN SYDFIELD ON PADR(SYDFLFLD.CFLD_NAME, 10) = PADR(SYDFIELD.CFLD_NAME, 10)";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            table.TableName = "SYDFLFLD";
            string[] pk = new string[2];
            pk[0] = "cfld_name";
            pk[1] = "cfile_nam";

            if (!File.Exists(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFLFLD.XML")))
            {
                DataTable tempTable = table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFLFLD.XML"), XmlWriteMode.WriteSchema);
            }

            Helpers.MergeTables(table, Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDFLFLD.XML"), pk);

            return table;
        }

        public override DataTable GetOptionGridDataTable()
        {
            OdbcCommand tableCommand = new OdbcCommand();
            tableCommand.CommandText = "Select DISTINCT crep_id,crep_name,capobjtyp,CUPGRDLVL from sydreprt order by crep_id";
            tableCommand.Connection = new OdbcConnection(this.ConnectionString);
            tableCommand.Connection.Open();

            OdbcDataAdapter tableAdapter = new OdbcDataAdapter(tableCommand);
            DataTable table = new DataTable();

            tableAdapter.Fill(table);
            tableCommand.Connection.Close();

            table.TableName = "sydreprt";
            string[] pk = new string[1];
            pk[0] = "crep_id";

            if (!File.Exists(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDREPRT.XML")))
            {
                DataTable tempTable = table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYDREPRT.XML"), XmlWriteMode.WriteSchema);
            }

            return table;
        }

        public override DataTable GetOptionGridVariableDataTable()
        {
            OdbcCommand command = new OdbcCommand();
            command.CommandText = "SELECT DISTINCT CUPGRDLVL,crep_ID,mventries,nVarPos,cbrwselfld,mbrwfields,mbrwfltexp,cdefa_typ,mdata_def,cassociate,msupexpr,csetfunc,cCodes_fld,cdata_typ,nfld_dec,mfld_des,mfld_name,cfld_head,cpict_str,cfld_msg,lvldentry,mvald_str,nfld_wdth from syrepuvr";
            command.Connection = new OdbcConnection(this.ConnectionString);
            command.Connection.Open();

            OdbcDataAdapter adapter = new OdbcDataAdapter(command);
            DataTable table = new DataTable();

            adapter.Fill(table);
            command.Connection.Close();

            table.TableName = "syrepuvr";
            string[] pk = new string[2];
            pk[0] = "crep_id";
            pk[1] = "cfld_head";

            if (!File.Exists(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYREPUVR.XML")))
            {
                DataTable tempTable = table.Copy();
                tempTable.Rows.Clear();
                tempTable.Columns.Add("ModeficationType", typeof(string));
                tempTable.Columns.Add("_Reason", typeof(string));
                tempTable.WriteXml(Path.Combine(Path.Combine(Application.StartupPath, MergePath), "SYREPUVR.XML"), XmlWriteMode.WriteSchema);
            }

            return table;
        }
    }
}
