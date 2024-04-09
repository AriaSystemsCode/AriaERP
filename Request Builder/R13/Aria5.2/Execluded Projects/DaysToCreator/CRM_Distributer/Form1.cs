using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Data.SqlClient;  

namespace CRM_Distributer
{
    public partial class Form1 : Form
    {
        public bool Stop = false;

        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {

            

            //System.Data.SqlClient.SqlConnection con = new SqlConnection(@"server=ARIA-HASSAN\SQLEXPRESS;database=System.Master;Integrated Security=True;");
            String Conn = "server=" + textBox1.Text.ToString()+";database=System.Master;";

            if (checkBox1.Checked == true)
            { Conn = Conn + "Integrated Security=True;"; }
            else
            { Conn = Conn + "User id=" + textBox2.Text.ToString() + "; Password=" + textBox3.Text.ToString() + ";"; }

              System.Data.SqlClient.SqlConnection con = new SqlConnection(@Conn );
            //SqlCommand cmd = new SqlCommand("SELECT* FROM AriaObjectProperty WHERE (PropertySettings LIKE '%<_dataType>Date</_dataType>%') AND (NOT (PropertyName IN ('EditDate', 'LockDate', 'AddDate')))");

            SqlCommand cmd = new SqlCommand("SELECT     AriaObjectProperty.ObjectID, AriaObjectProperty.ObjectRevision, AriaObjectProperty.PropertyName, AriaObjectProperty.ModificationType, AriaObjectProperty.PropertyType, AriaObjectProperty.PropertySettings, AriaObjectRevision.ObjectRevisionSettings FROM         AriaObjectProperty INNER JOIN AriaObjectRevision ON AriaObjectProperty.ObjectID = AriaObjectRevision.ObjectID WHERE     (AriaObjectProperty.PropertySettings LIKE '%<_dataType>Date</_dataType>%') AND PropertyType='AriaField' And(NOT (AriaObjectProperty.PropertyName IN ('EditDate', 'LockDate', 'AddDate')))");

            con.Open(); 
            cmd.Connection = con;

            DataTable dbtb = new DataTable();
            SqlDataAdapter dap = new SqlDataAdapter (cmd);
            dap.Fill(dbtb);

            if (dbtb.Rows.Count > 0)
            {
                button2.Enabled = true;
                progressBar1.Maximum = dbtb.Rows.Count+1;
                progressBar1.Value = 1;
                progressBar1.Visible = true;
                label4.Text ="";
                label4.Visible = true ;
                Stop = false;
                try
                {
                    if (checkBox2.Checked == true)
                    {
                        cmd.CommandText = "delete FROM AriaObjectProperty WHERE (PropertyName LIKE 'DaysTo%')";
                        cmd.ExecuteNonQuery(); 
                        MessageBox.Show("Old DaysTo Records Are Deleted ");
                    };
                }
                    catch (Exception ex1)
                { MessageBox.Show(ex1.Message); }
                
            }



            int iRow = 0;
            foreach (DataRow row in dbtb.Rows)
            {   iRow = iRow + 1;
                progressBar1.Value = iRow;
                
                String strXML = row["PropertySettings"].ToString();
                //Handle the DataType tag [Start]
                strXML = strXML.Replace("<_dataType>Date</_dataType>", "<_dataType>Numeric</_dataType>");
                //Handle the DataType tag [End]

                //Handle the Width tag [Start]
                strXML = strXML.Replace("<_width>8</_width>", "<_width>6</_width>");
                //Handle the Width tag [End]

                 
                //Handle the Head tag [Start]
                int headint = row["PropertySettings"].ToString().IndexOf("</_head>");
                int headint2 = strXML.Substring(0, headint).LastIndexOf(">");
                strXML = strXML.Substring(0, headint2 + 1) + "Days to " + strXML.Substring((headint2 + 1)); 
                
                //Handle the Head tag [End]

                //Handle the Field Name tag [Start]

                String TableName = row["ObjectRevisionSettings"].ToString().Substring(0, row["ObjectRevisionSettings"].ToString().IndexOf("</_tableName>") );
                TableName = TableName.Substring(TableName.LastIndexOf(">")+1);  

                int headint1 = row["PropertySettings"].ToString().IndexOf("</_fieldName>");

                headint2 = strXML.Substring(0, headint1).LastIndexOf(">");
                String FieldName = strXML.Substring(headint2 + 1, headint1 - headint2 -1 );
                strXML = strXML.Substring(0, headint2 + 1) + "DATEDIFF(day, " + TableName + "." + FieldName + ", GetDate())" + strXML.Substring((headint1)); 
                //Handle the Field Name tag [End]

                label4.Text = TableName + '.' + FieldName;

                //SAB
                if (TableName == "PMPRJDT" && (FieldName == "DCLC_FNSH" || FieldName == "DCLC_STRT"))
                {
                    
                }
                else
                {
                    continue;
                }
                //SAB

                String str = "insert into AriaObjectProperty ( ObjectID, ObjectRevision, PropertyName, ModificationType, PropertyType, PropertySettings) values (" + row["ObjectID"].ToString() + "," + row["ObjectRevision"].ToString() + " ,'" + "DaysTo" + row["PropertyName"].ToString() + "' ,'" + row["ModificationType"].ToString() + "' ,'" + row["PropertyType"].ToString() + "' ,'" + strXML + "' )";
                
                cmd.CommandText = str;
                cmd.ExecuteNonQuery();
                button2.Enabled = true ;
                if (Stop == true)
                { break; }
 
            };

            con.Close();
            button2.Enabled = false ;
            progressBar1.Visible = false;
            label4.Visible = false;
            MessageBox.Show("Daysto Fields Creation Is Done [ "+dbtb.Rows.Count.ToString()   +"]"); 
            }
            catch (Exception ex)
            {
                button2.Enabled = false;
                progressBar1.Visible = false;
                label4.Visible = false;
                MessageBox.Show(ex.Message);  
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Stop = true;
        }
    }
}
