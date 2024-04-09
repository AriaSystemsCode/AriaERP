using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Data.SqlClient;
using System.Xml;

namespace Mapping
{
    public partial class Form1 : Form
    {
        DataSet ds; Form2 form2;
        XmlDocument XMlMap;
        public Form1()
        {
            InitializeComponent();
            form2 = new Form2();
            InitXML();
            FillSchema(@"C:\t\OrderReport.xsd");
        }

        private void btnOpen_Click(object sender, EventArgs e)
        {
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                txtSchemaPath.Text = openFileDialog1.FileName;
                if (txtSchemaPath.Text != "" && File.Exists(txtSchemaPath.Text))
                {
                    FillSchema(txtSchemaPath.Text);
                }
                else
                    MessageBox.Show("Please Check schema file");
            }
            else
                txtSchemaPath.Text = "";

        }

        private void FillSchema(string schemaPath)
        {
            ds = new DataSet();
            ds.ReadXmlSchema(schemaPath);
            treesource.Nodes.Clear();
            foreach (DataTable table in ds.Tables)
            {
                treesource.Nodes.Add(table.TableName);
            }
        }

        private void btnServerLoad_Click(object sender, EventArgs e)
        {
            SqlConnection con = GetCon();
            SqlCommand cmd = con.CreateCommand();
            cmd.CommandText = "SELECT * FROM sys.Tables";
            DataTable table = new DataTable();
            SqlDataAdapter da = new SqlDataAdapter(cmd);
            da.Fill(table);
            treeDist.Nodes.Clear();
            foreach (DataRow row in table.Rows)
            {
                treeDist.Nodes.Add(row["name"].ToString());
            }
        }

        private void treeSchemaTableColumns_NodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            string table = e.Node.Text;
            e.Node.Nodes.Clear();
            if (ds.Tables.Contains(table))
            {
                foreach (DataColumn column in ds.Tables[table].Columns)
                {
                    e.Node.Nodes.Add(column.Caption);
                }
                e.Node.ExpandAll();
            }
        }

        private SqlConnection GetCon()
        {
            return form2.Connection();
        }

        private void btnConfigure_Click(object sender, EventArgs e)
        {
            form2.ShowDialog();
        }

        private void treeServerTableColumns_NodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                string TableName = e.Node.Text;
                SqlCommand cmd = GetCon().CreateCommand();
                cmd.CommandText = "select column_name,data_type from information_schema.columns where table_name = '" + TableName + "' order by ordinal_position";
                DataTable table = new DataTable();
                SqlDataAdapter da = new SqlDataAdapter(cmd);
                da.Fill(table);
                e.Node.Nodes.Clear();
                foreach (DataRow row in table.Rows)
                {
                    e.Node.Nodes.Add(row["column_name"].ToString());
                }
                e.Node.ExpandAll();
            }
        }

        private void treeSchemaTableColumns_DragDrop(object sender, DragEventArgs e)
        {
            TreeView tree = (TreeView)sender;
            Point pt = new Point(e.X, e.Y);
            pt = tree.PointToClient(pt);
            TreeNode nodeTarget = tree.GetNodeAt(pt);
            if (nodeTarget.Level == 1)
            {
                TreeNode nodeSource = (TreeNode)e.Data.GetData(typeof(TreeNode));
                nodeTarget.Nodes.Add((TreeNode)nodeSource.Clone());
                nodeTarget.Expand();


                string TableSrc, FieldSrc, TableDest, FieldDest;
                TableSrc = nodeSource.Parent.Text;
                FieldSrc = nodeSource.Text;
                TableDest = nodeTarget.Parent.Text;
                FieldDest = nodeTarget.Text;
                if (nodeSource.TreeView.Name.ToLower() == "treesource")
                    Map(TableSrc, FieldSrc, TableDest, FieldDest, "", "");
                else
                    Map(TableDest, FieldDest, TableSrc, FieldSrc, "", "");
            }
        }

        private void treeSchemaTableColumns_MouseDown(object sender, MouseEventArgs e)
        {
            TreeView tree = (TreeView)sender;
            TreeNode node = tree.GetNodeAt(e.X, e.Y);
            if (node.Level == 1)
            {
                tree.SelectedNode = node;

                if (node != null)
                {
                    tree.DoDragDrop(node, DragDropEffects.Copy);
                }
            }
        }

        private void treeSchemaTableColumns_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.None;

            TreeView tree = (TreeView)sender;
            TreeNode nodeSource = (TreeNode)e.Data.GetData(typeof(TreeNode));
            if (nodeSource != null)
            {
                if (nodeSource.TreeView != tree)
                {
                    Point pt = new Point(e.X, e.Y);
                    pt = tree.PointToClient(pt);
                    TreeNode nodeTarget = tree.GetNodeAt(pt);
                    if (nodeTarget != null)
                    {
                        e.Effect = DragDropEffects.Copy;
                        tree.SelectedNode = nodeTarget;
                    }
                }
            }
        }

        private void InitXML()
        {
            XMlMap = new XmlDocument();
            XmlDeclaration dec = XMlMap.CreateXmlDeclaration("1.0", "", "");
            XMlMap.AppendChild(dec);
            XmlNode root = XMlMap.CreateNode(XmlNodeType.Element, "Root", "");
            XMlMap.AppendChild(root);
        }

        private void Map(string Table1, string Field1, string Table2, string Field2, string Fixed, string Condition)
        {

            XmlNode Map = XMlMap.CreateNode(XmlNodeType.Element, "Map", "");
            XmlNode srcTable = XMlMap.CreateNode(XmlNodeType.Element, "SrcTable", "");
            srcTable.InnerText = Table1;

            XmlNode srcField = XMlMap.CreateNode(XmlNodeType.Element, "srcField", "");
            srcField.InnerText = Field1;

            XmlNode DestTable = XMlMap.CreateNode(XmlNodeType.Element, "DestTable", "");
            DestTable.InnerText = Table2;

            XmlNode DestField = XMlMap.CreateNode(XmlNodeType.Element, "DestField", "");
            DestField.InnerText = Field2;

            XmlNode ConditionNode = XMlMap.CreateNode(XmlNodeType.Element, "Condition", "");
            ConditionNode.InnerText = Condition;

            XmlNode FixedNode = XMlMap.CreateNode(XmlNodeType.Element, "Fixed", "");
            FixedNode.InnerText = Fixed;

            Map.AppendChild(srcTable);
            Map.AppendChild(srcField);
            Map.AppendChild(DestTable);
            Map.AppendChild(DestField);
            Map.AppendChild(ConditionNode);
            Map.AppendChild(FixedNode);

            XMlMap["Root"].AppendChild(Map);
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            if (saveFileDialog1.ShowDialog() == DialogResult.OK)
            {
                XMlMap.Save(saveFileDialog1.FileName);
            }
        }

        private void treeDist_NodeMouseClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            if (e.Button == MouseButtons.Right && e.Node.Level == 1)
            {
                e.Node.TreeView.SelectedNode = e.Node;
                contextMenuStrip1.Show(e.Node.TreeView, new Point(e.X, e.Y));
            }
        }

        private void addConditionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            lblCondition.Visible = true;
            txtCondition.Visible = true;
            txtCondition.Text = "";
            txtCondition.Focus();
            btnAddCondition.Visible = true;
        }

        private void fixedValueToolStripMenuItem_Click(object sender, EventArgs e)
        {
            lblTitle.Visible = true;
            txtValue.Visible = true;
            txtValue.Text = "";
            btnAdd.Visible = true;
            txtValue.Focus();
        }

        private void btnAdd_Click(object sender, EventArgs e)
        {
            if (txtValue.Text != "")
            {
                TreeNode nodeTarget = treeDist.SelectedNode;
                nodeTarget.Nodes.Add("Fixed:" + txtValue.Text);
                nodeTarget.ExpandAll();
                string TableDest = nodeTarget.Parent.Text;
                string FieldDest = nodeTarget.Text;
                Map("", "", TableDest, FieldDest, txtValue.Text, "");

                lblTitle.Visible = false;
                txtValue.Visible = false;
                btnAdd.Visible = false;
            }
        }

        private void btnAddCondition_Click(object sender, EventArgs e)
        {
            if (txtCondition.Text != "")
            {
                TreeNode nodeTarget = treeDist.SelectedNode;
                nodeTarget.Nodes.Add("Condition:" + txtCondition.Text);
                nodeTarget.ExpandAll();
                string TableDest = nodeTarget.Parent.Text;
                string FieldDest = nodeTarget.Text;
                Map("", "", TableDest, FieldDest, "", txtCondition.Text);

                lblCondition.Visible = false;
                txtCondition.Visible = false;
                btnAddCondition.Visible = false;
            }
        }

        private void txtCondition_DragDrop(object sender, DragEventArgs e)
        {
            TreeNode nodeSource = (TreeNode)e.Data.GetData(typeof(TreeNode));
            txtCondition.Text += nodeSource.Parent.Text + "." + nodeSource.Text;
        }

        private void txtCondition_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
    }
}
