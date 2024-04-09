using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Data.SqlClient;

namespace Mapping
{
    public partial class Form2 : Form
    {
        public Form2()
        {
            InitializeComponent();
        }

        private void btnApply_Click(object sender, EventArgs e)
        {
            SqlConnection con = GetCon();
            try
            {
                con.Open();
                this.Hide();
            }
            catch (Exception ex)
            {
                MessageBox.Show("Error Connecting to SQL");
            }
        }

        private SqlConnection GetCon()
        {
            SqlConnection con = new SqlConnection();
            con.ConnectionString = "Data Source=" + txtServerName.Text + ";Initial Catalog=" + txtDBname.Text + ";";
            if (txtUserName.Text != "")
                con.ConnectionString += "User Id=" + txtUserName.Text + ";Password=" + txtPassword.Text + ";";
            else
                con.ConnectionString += "Trusted_Connection=True";
            return con;
        }

        public SqlConnection Connection()
        {
            SqlConnection con = GetCon();
            try
            {
                con.Open();
                return con;
            }
            catch (Exception ex)
            {
                throw new Exception("Error Connecting to SQL");
            }
        }
    }
}
