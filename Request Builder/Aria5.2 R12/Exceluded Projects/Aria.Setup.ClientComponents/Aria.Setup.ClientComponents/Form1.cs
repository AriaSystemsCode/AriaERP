using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using System.IO;
using System.Diagnostics;
using System.Reflection;
using Microsoft.Win32;
using System.Threading;

namespace Aria.Setup.ClientComponents
{
    public partial class Form1 : Form
    {
        private ArrayList assemblyNames = new ArrayList();

        public Form1()
        {
            InitializeComponent();

            if (isValidAria4Path(Application.StartupPath))
            {
                textBox1.Text = Application.StartupPath;
                fillDllsTable(true);
            }
        }

        private void fillDllsTable(bool makeCopy)
        {
            DllInfo[] dlls = loadCurrentDlls(textBox1.Text,makeCopy);
            DataTable table = new DataTable();
            table.Columns.Add("DLL Name");
            table.Columns.Add("Registered in GAC");
            table.Columns.Add("Registered Assembly");

            foreach (DllInfo dll in dlls)
            {
                DataRow row = table.NewRow();
                row[0] = dll.DllLongName;
                row[1] = (dll.IsGacRegistered ? "Yes" : "No");
                row[2] = (dll.IsRegisteredAssembly ? "Yes" : "No");
                
                table.Rows.Add(row);
            }

            dataGridView1.DataSource = table;
            dataGridView1.Columns[1].Width = 300;
        }

        private DllInfo[] loadCurrentDlls(string aria4Path,bool makeCopy)
        {
            DataSet dataSet = new DataSet();
            dataSet.ReadXml(aria4Path+"\\ASM.xml");
            DataTable table = dataSet.Tables[0];

            ArrayList dlls = new ArrayList();
            assemblyNames = new ArrayList();

            foreach (DataRow row in table.Rows)
            {
                DllInfo dll = new DllInfo();
                dll.DllShortName = row["dll_short_name"].ToString();
                dll.DllLongName = row["dll_long_name"].ToString();

                if (makeCopy)
                {
                    FileInfo file = new FileInfo(aria4Path + "\\OCXS\\" + dll.DllShortName);
                    file.CopyTo(aria4Path + "\\OCXS\\" + dll.DllLongName, true);
                }
                
                Assembly assembly = Assembly.LoadFile(aria4Path + "\\OCXS\\" + dll.DllLongName);
                dll.IsGacRegistered = assembly.GlobalAssemblyCache;
                dll.IsRegisteredAssembly = true;
                foreach (Type t in assembly.GetExportedTypes())
                {
                    dll.IsRegisteredAssembly = dll.IsRegisteredAssembly && (Registry.ClassesRoot.OpenSubKey(t.FullName) != null);
                }

                dlls.Add(dll);
                assemblyNames.Add(aria4Path + "\\OCXS\\" + dll.DllLongName);
            }

            return (DllInfo[])dlls.ToArray(typeof(DllInfo));
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Enabled = false;
            folderBrowserDialog1.ShowDialog();
            
            try
            {
                if (!isValidAria4Path(folderBrowserDialog1.SelectedPath))
                    throw new Exception();

                textBox1.Text = folderBrowserDialog1.SelectedPath;
                fillDllsTable(true);
            }
            catch
            {
                dataGridView1.DataSource = null;
                MessageBox.Show("You choosed invalid Aria4XP directory");
            }

            this.Enabled = true;
        }
        private bool isValidAria4Path(string path)
        {
            DirectoryInfo directory = new DirectoryInfo(path);

            if (!directory.Name.ToLower().Equals("aria4xp"))
                return false;

            if (directory.GetDirectories("[oO][cC][xX][sS]") == null)
                return false;

            return true;
        }

        private void registerDll(string dllFullPath)
        {
            try
            {
                Process process = new Process();
                process.StartInfo = new ProcessStartInfo("GACUTIL", "/if " + dllFullPath);
                process.StartInfo.UseShellExecute = false;
                process.Start();
            }
            catch(Exception e)
            {
                MessageBox.Show(e.Message);
            }

            for (int i = 0; i < 2; i++)
            {
                try
                {
                    string tlb = dllFullPath.Replace(".dll", ".tlb");
                    Process process = new Process();
                    process.StartInfo = new ProcessStartInfo("REGASM", dllFullPath + " /tlb:" + tlb);
                    process.StartInfo.CreateNoWindow = true;
                    process.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
                    process.Start();
                }
                catch (Exception e)
                {
                    MessageBox.Show(e.Message);
                }
            }

            Thread.Sleep(1000);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                int i = -1;
                foreach (DataGridViewRow row in dataGridView1.Rows)
                {
                    i++;
                    if (row.Cells[0].Value == null)
                        continue;

                    else if ((bool)row.Cells[0].Value == false)
                        continue;

                    registerDll(assemblyNames[i].ToString());
                }

                fillDllsTable(false);
                fillDllsTable(false);
            }
            catch(Exception exc)
            {
                MessageBox.Show(exc.Message);
            }
        }
    }
}