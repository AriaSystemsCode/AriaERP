using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using AriaObjectBrowser.DataTypes;
using AriaObjectBrowser.Enums;
using AriaObjectBrowser.UserControls;
using System.Xml.Serialization;
using System.Runtime.InteropServices;
using AriaObjectBrowser.Properties;
using Microsoft.Win32;
using TaskPaneControl;

namespace AriaObjectBrowser.Forms
{
    public partial class AriaObjectBrowserMain : Form
    {
        private AriaSettings _settings;

        public AriaSettings Settings
        {
            get { return _settings; }
            set
            {
                _settings = value;
                _settings.Changed += new AriaSettingChangedEventHandler(AriaSettings_Changed);
            }
        }

        private object _ariaApplication;
        public object AriaApplication
        {
            get { return _ariaApplication; }
            set
            {
                _ariaApplication = value;
                objectsContainer.AriaApplication = _ariaApplication;
                singleFileControl.AriaApplication = _ariaApplication;
            }
        }

        private object _ariaFrom;
        public object AriaFrom
        {
            get { return _ariaFrom; }
            set { _ariaFrom = value; }
        }

        private object _interOperability;
        public object InterOperability
        {
            get { return _interOperability; }
            set { _interOperability = value; }
        }

        private string _objectType;
        public string ObjectType
        {
            get { return _objectType; }
            set { _objectType = value; }
        }

        private string _objectKey;
        public string ObjectKey
        {
            get { return _objectKey; }
            set { _objectKey = value; }
        }

        private string _connectionstring;
        public string Connectionstring
        {
            get { return _connectionstring; }
            set { _connectionstring = value; }
        }

        //Moh 21-7-2009 Add property to hold objects save path start
        private string _ObjectsPath;
        public string ObjectsPath
        {
            get { return _ObjectsPath; }
            set { _ObjectsPath = value; }
        }

        //Moh 21-7-2009 Add property to hold objects save path End

        private AriaObjectList _objects = new AriaObjectList();

        public AriaObjectBrowserMain()
        {
            InitializeComponent();

            Settings = new AriaSettings();

            _objects.Settings = Settings;

            objectsContainer.Settings = _settings;
            singleFileControl.Settings = _settings;

            splitContainerRight.SplitterDistance = Convert.ToInt32(Convert.ToDouble(splitContainerRight.Width) * 0.80);

            objectsContainer.AriaObjects = _objects;

            _objects.ActiveObjectChanged += new AriaActiveObjectChangedEventHandler(_objects_ActiveObjectChanged);

            nextClipboardViewer = (IntPtr)SetClipboardViewer((int)this.Handle);

            CheckClipboard();

            Settings setting = new Settings();
            //MOH 21-7-2009 Fix Error if registery keys doesn't exist Start
            //setting.View = Registry.GetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "View", "").ToString();
            //if (setting.View != null && setting.View.Trim().Length > 0)
            //{
            //    _settings.ViewType = (AriaViewType)Enum.Parse(typeof(AriaViewType), setting.View);
            //}

            //setting.Zoom = Registry.GetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "Zoom", "").ToString();
            //if (setting.Zoom != null && setting.Zoom.Trim().Length > 0)
            //{
            //    _settings.Zoom = Convert.ToInt32(setting.Zoom);
            //}

            //setting.SortBy = Registry.GetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "SortBy", "").ToString();
            //if (setting.SortBy != null && setting.SortBy.Trim().Length > 0)
            //{   
            //    _settings.SortType = (AriaSortType)Enum.Parse(typeof(AriaSortType), setting.SortBy);
            //}

            //setting.ShowFileNames = Registry.GetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "ShowFileNames", "").ToString();
            //if (setting.ShowFileNames != null && setting.ShowFileNames.Trim().Length > 0)
            //{
            //    _settings.ShowFileNames = setting.ShowFileNames == "True" ? true : false;
            //}

            //setting.ShowTaskPane = Registry.GetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "ShowTaskPane", "").ToString();
            //if (setting.ShowTaskPane != null & setting.ShowTaskPane.Trim().Length > 0)
            //{
            //    _settings.ShowTaskPane = setting.ShowTaskPane == "True" ? true : false;
            //}

            setting.View = ReadRegistery(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "View");
            if (setting.View != null && setting.View.Trim().Length > 0)
            {
                _settings.ViewType = (AriaViewType)Enum.Parse(typeof(AriaViewType), setting.View);
            }

            setting.Zoom = ReadRegistery(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "Zoom");
            if (setting.Zoom != null && setting.Zoom.Trim().Length > 0)
            {
                _settings.Zoom = Convert.ToInt32(setting.Zoom);
            }

            setting.SortBy = ReadRegistery(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "SortBy");
            if (setting.SortBy != null && setting.SortBy.Trim().Length > 0)
            {
                _settings.SortType = (AriaSortType)Enum.Parse(typeof(AriaSortType), setting.SortBy);
            }

            setting.ShowFileNames = ReadRegistery(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "ShowFileNames");
            if (setting.ShowFileNames != null && setting.ShowFileNames.Trim().Length > 0)
            {
                _settings.ShowFileNames = setting.ShowFileNames == "True" ? true : false;
            }

            setting.ShowTaskPane = ReadRegistery(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "ShowTaskPane");
            if (setting.ShowTaskPane != null & setting.ShowTaskPane.Trim().Length > 0)
            {
                _settings.ShowTaskPane = setting.ShowTaskPane == "True" ? true : false;
            }
        }
        private string ReadRegistery(string KeyName, string ValueName)
        {
            string ret = "";
            try
            {
                object x = Registry.GetValue(KeyName, ValueName, "");
                ret = x != null ? x.ToString() : "";
            }
            catch { };
            return ret;

        }
        //MOH 21-7-2009 Fix Error if registery keys doesn't exist End
        public void LoadObjects()
        {
            string connection = _connectionstring;
            System.Data.Odbc.OdbcConnection con = new System.Data.Odbc.OdbcConnection(connection);
            //     con.Open();

            System.Data.Odbc.OdbcCommand comm = new System.Data.Odbc.OdbcCommand("select objects.*, objlink.cdef_obj from objlink join objects on (objlink.cobject_id = objects.cobject_id) where cobjlnktyp + cobjlink = '" + _objectType + _objectKey.PadRight(20) + "'", con);

            System.Data.Odbc.OdbcDataAdapter ad = new System.Data.Odbc.OdbcDataAdapter(comm);

            DataTable table = new DataTable();
            ad.Fill(table);

            for (int i = 0; i < table.Rows.Count; i++)
            {
                // MOH 21-7-2009 Fix error if file not exist or still in general field Start
                //AriaObject newObject = new AriaObject();
                //newObject.ObjectId = table.Rows[i]["cobject_id"].ToString().TrimEnd();
                //newObject.Name = table.Rows[i]["cobj_desc"].ToString().TrimEnd();
                //newObject.Notes = table.Rows[i]["mobj_notes"].ToString().TrimEnd();
                //newObject.File = new FileInfo(table.Rows[i]["mimgpath"].ToString().TrimEnd());

                //_objects.AddObject(newObject);
                string filePath = table.Rows[i]["mimgpath"].ToString().TrimEnd();
                if (!File.Exists(filePath)) continue;
                string ObjectId = table.Rows[i]["cobject_id"].ToString().TrimEnd();
                if (!filePath.Contains(ObjectsPath))
                {
                    FileInfo file = new FileInfo(filePath);
                    filePath = ObjectsPath + file.Name;
                    if (!File.Exists(filePath)) file.CopyTo(filePath);
                    comm.CommandText = "Update objects set mimgpath='" + filePath + "' where cobject_id == '" + ObjectId + "'";
                    comm.Connection.Open();
                    comm.ExecuteNonQuery();
                    comm.Connection.Close();
                }
                AriaObject newObject = new AriaObject();
                newObject.ObjectId = ObjectId;
                newObject.Name = table.Rows[i]["cobj_desc"].ToString().TrimEnd();
                newObject.Notes = table.Rows[i]["mobj_notes"].ToString().TrimEnd();
                newObject.File = new FileInfo(filePath);

                _objects.AddObject(newObject);
                // MOH 21-7-2009 Fix error if file not exist or still in general field End

                if (table.Rows[i]["cdef_obj"].ToString().TrimEnd() == "D")
                {
                    _objects.DefaultObjectIndex = i;
                }
            }
        }

        private void AriaObjectBrowserMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            Settings setting = new Settings();
            setting.View = _settings.ViewType.ToString();
            Registry.SetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "View", setting.View);
            setting.Zoom = _settings.Zoom.ToString();
            Registry.SetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "Zoom", setting.Zoom);
            setting.SortBy = _settings.SortType.ToString();
            Registry.SetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "SortBy", setting.SortBy);
            setting.ShowFileNames = _settings.ShowFileNames.ToString();
            Registry.SetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "ShowFileNames", setting.ShowFileNames);
            setting.ShowTaskPane = _settings.ShowTaskPane.ToString();
            Registry.SetValue(@"HKEY_CURRENT_USER\Software\Aria\5.0\ObjectBrowser", "ShowTaskPane", setting.ShowTaskPane);
            setting.Save();

            InterOper interOper = new InterOper();
            interOper.RefreshAll(InterOperability, AriaFrom, _objectType, _objectKey);
        }

        #region Aria Settings
        void AriaSettings_Changed()
        {
            #region "View Menu"
            thumbnailsToolStripMenuItem.Checked = _settings.ViewType == AriaViewType.Thumbnail;
            filmstripToolStripMenuItem.Checked = _settings.ViewType == AriaViewType.Filmstrip;
            singleFileToolStripMenuItem.Checked = _settings.ViewType == AriaViewType.SingleFile;

            taskPaneToolStripMenuItem.Checked = _settings.ShowTaskPane;

            sortByFileNameToolStripMenuItem.Checked = _settings.SortType == AriaSortType.FileName;
            sortByDateToolStripMenuItem.Checked = _settings.SortType == AriaSortType.Date;
            sortByFileTypeToolStripMenuItem.Checked = _settings.SortType == AriaSortType.FileType;
            sortByFileSizeToolStripMenuItem.Checked = _settings.SortType == AriaSortType.FileSize;

            showFileNamesToolStripMenuItem.Checked = _settings.ShowFileNames;
            #endregion

            #region "View Tool Strip"
            thumbnailsToolStripButton.Checked = _settings.ViewType == AriaViewType.Thumbnail;
            filmstripToolStripButton.Checked = _settings.ViewType == AriaViewType.Filmstrip;
            singleFileToolStripButton.Checked = _settings.ViewType == AriaViewType.SingleFile;
            #endregion

            #region View
            switch (_settings.ViewType)
            {
                case AriaViewType.Thumbnail:
                    splitContainerLeft.IsSplitterFixed = false;

                    splitContainerLeft.Panel1Collapsed = false;
                    splitContainerLeft.Panel2Collapsed = true;
                    break;

                case AriaViewType.Filmstrip:
                    splitContainerLeft.IsSplitterFixed = true;

                    splitContainerRight.SplitterDistance = Convert.ToInt32(Convert.ToDouble(splitContainerRight.Width) * 0.80);

                    splitContainerLeft.Panel2Collapsed = false;
                    splitContainerLeft.Panel1Collapsed = false;
                    break;

                case AriaViewType.SingleFile:
                    splitContainerLeft.IsSplitterFixed = false;

                    splitContainerLeft.Panel2Collapsed = false;
                    splitContainerLeft.Panel1Collapsed = true;
                    break;

            }
            #endregion

            #region "Task Pane"
            taskPane.Visible = _settings.ShowTaskPane;
            splitContainerRight.Panel2Collapsed = !_settings.ShowTaskPane;
            #endregion

            #region "Statusbar"
            zoomToolStripComboBox.Text = _settings.Zoom.ToString() + "%";
            trackBar.Value = _settings.Zoom;
            movePreviousButton.Enabled = _objects.Count > 1 && _objects.ActiveObjectIndex > 0;
            moveNextButton.Enabled = _objects.Count > 1 && _objects.ActiveObjectIndex < _objects.Count - 1;
            #endregion
        }
        #endregion

        #region Clipboard Notify
        [DllImport("User32.dll")]
        protected static extern int
                  SetClipboardViewer(int hWndNewViewer);

        [DllImport("User32.dll", CharSet = CharSet.Auto)]
        public static extern bool
               ChangeClipboardChain(IntPtr hWndRemove,
                                    IntPtr hWndNewNext);

        [DllImport("user32.dll", CharSet = CharSet.Auto)]
        public static extern int SendMessage(IntPtr hwnd, int wMsg,
                                             IntPtr wParam,
                                             IntPtr lParam);

        IntPtr nextClipboardViewer;

        public void CheckClipboard()
        {
            XmlSerializer xmlSerializer = new XmlSerializer(typeof(AriaClipboardObject));

            try
            {
                TextReader textRead = new StringReader(Clipboard.GetText());
                AriaClipboardObject ariaObject = (AriaClipboardObject)xmlSerializer.Deserialize(textRead);
                pasteToolStripMenuItem.Enabled = true;
                pasteToolStripButton.Enabled = true;
            }
            catch (Exception)
            {
                pasteToolStripMenuItem.Enabled = false;
                pasteToolStripButton.Enabled = false;
            }
        }

        protected override void WndProc(ref System.Windows.Forms.Message m)
        {
            // defined in winuser.h
            const int WM_DRAWCLIPBOARD = 0x308;
            const int WM_CHANGECBCHAIN = 0x030D;

            switch (m.Msg)
            {
                case WM_DRAWCLIPBOARD:
                    CheckClipboard();
                    SendMessage(nextClipboardViewer, m.Msg, m.WParam,
                                m.LParam);

                    break;

                case WM_CHANGECBCHAIN:
                    if (m.WParam == nextClipboardViewer)
                        nextClipboardViewer = m.LParam;
                    else
                        SendMessage(nextClipboardViewer, m.Msg, m.WParam,
                                    m.LParam);
                    break;

                default:
                    base.WndProc(ref m);
                    break;
            }
        }
        #endregion

        #region Aria Object List
        void _objects_ActiveObjectChanged()
        {
            #region File Menu
            printToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0 && _objects[_objects.ActiveObjectIndex].IsPicture;
            #endregion

            #region Edit Menu
            cutToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            copyToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            editToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            deleteToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            renameToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            changeNotesToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            markAsDefaultToolStripMenuItem.Enabled = _objects.ActiveObjectIndex >= 0;
            #endregion

            #region Standard Toolbar
            printToolStripButton.Enabled = _objects.ActiveObjectIndex >= 0 && _objects[_objects.ActiveObjectIndex].IsPicture;
            cutToolStripButton.Enabled = _objects.ActiveObjectIndex >= 0;
            copyToolStripButton.Enabled = _objects.ActiveObjectIndex >= 0;
            editToolStripButton.Enabled = _objects.ActiveObjectIndex >= 0;
            deleteToolStripButton.Enabled = _objects.ActiveObjectIndex >= 0;
            markAsDefaultToolStripButton.Enabled = _objects.ActiveObjectIndex >= 0;
            #endregion

            #region Task Pane
            #region Properties Task Pane
            propertiesFileNameLabel.Text = "No Selected File";
            propertiesFileTypeLabel.Text = "Type:";
            propertiesFileSizeLabel.Text = "Size:";
            propertiesFileModifiedLabel.Text = "Modified:";
            propertiesFileLocation.Text = "";

            if (_objects.ActiveObjectIndex >= 0 && File.Exists(_objects[_objects.ActiveObjectIndex].File.FullName))
            {
                propertiesFileNameLabel.Text = _objects[_objects.ActiveObjectIndex].Name;
                propertiesFileTypeLabel.Text = "Type: " + (_objects[_objects.ActiveObjectIndex].File.Extension.StartsWith(".") ?
                                                            _objects[_objects.ActiveObjectIndex].File.Extension.Substring(1) :
                                                                _objects[_objects.ActiveObjectIndex].File.Extension);
                propertiesFileSizeLabel.Text = "Size: " + Convert.ToString(_objects[_objects.ActiveObjectIndex].File.Length) + " Bytes";
                propertiesFileModifiedLabel.Text = "Modified: " + _objects[_objects.ActiveObjectIndex].File.LastWriteTime.ToShortDateString() + " " +
                                                                    _objects[_objects.ActiveObjectIndex].File.LastWriteTime.ToShortTimeString();
                propertiesFileLocation.Text = _objects[_objects.ActiveObjectIndex].File.FullName;
                propertiesFileNotes.Text = _objects[_objects.ActiveObjectIndex].Notes;
            }

            #endregion

            #region Rename Task Pane
            renameFileNameLabel.Text = "No Selected File";
            renameFileName.Enabled = _objects.ActiveObjectIndex >= 0;
            renameOk.Enabled = _objects.ActiveObjectIndex >= 0;

            if (_objects.ActiveObjectIndex >= 0)
            {
                renameFileNameLabel.Text = _objects[_objects.ActiveObjectIndex].Name;
                renameFileName.Text = _objects[_objects.ActiveObjectIndex].Name;
            }
            #endregion

            #region Change Notes Task Pane
            changeNotesFileNameLabel.Text = "No Selected File";
            changeNotesNotes.Enabled = _objects.ActiveObjectIndex >= 0;
            changeNotesOk.Enabled = _objects.ActiveObjectIndex >= 0;

            if (_objects.ActiveObjectIndex >= 0)
            {
                changeNotesFileNameLabel.Text = _objects[_objects.ActiveObjectIndex].Name;
                changeNotesNotes.Text = _objects[_objects.ActiveObjectIndex].Notes;
            }
            #endregion
            #endregion

            movePreviousButton.Enabled = _objects.Count > 1 && _objects.ActiveObjectIndex > 0;
            moveNextButton.Enabled = _objects.Count > 1 && _objects.ActiveObjectIndex < _objects.Count - 1;

            singleFileControl.SetSource(_objects, _objects.ActiveObjectIndex);
        }
        #endregion

        #region File Menu
        private void addToolStripMenuItem_Click(object sender, EventArgs e)
        {
            // MOH 21-7-2009 Save object to DBFS\company\objects folder Start
            //openFileDialog.ShowDialog();

            //if (File.Exists(openFileDialog.FileName))
            //{
            //    AriaObject currentObject = new AriaObject();

            //    InterOper interOper = new InterOper();
            //    currentObject.ObjectId = interOper.GetNewObjectKey(InterOperability);
            //    currentObject.File = new FileInfo(openFileDialog.FileName);
            //    currentObject.Name = currentObject.File.Name.Replace(currentObject.File.Extension, "");

            //    _objects.AddObject(currentObject);

            //    _objects.ActiveObjectIndex = _objects.Count - 1;

            //    interOper.AddNewObject(InterOperability, currentObject.ObjectId, currentObject.Name, currentObject.Notes, currentObject.File.FullName, currentObject.SftFormatFileName);
            //    interOper.AddNewObjectLink(InterOperability, _objectType, _objectKey, currentObject.ObjectId);
            //}

            openFileDialog.ShowDialog();
            if (File.Exists(openFileDialog.FileName))
            {
                Boolean addFile = true;
                FileInfo file = new FileInfo(openFileDialog.FileName);
                #region SaveToObjectsFolder
                string path = ObjectsPath + file.Name;
                // Hassan T20100331.0004 2010-05-02 [Begin]
                //if (!File.Exists(path))
                //{ file.CopyTo(path); };

                if (!File.Exists(path))
                { file.CopyTo(path); }
                else
                {
                    if (MessageBox.Show("A file with the same name already exist in the objects directory.(Overwrite it)", "Confirm Add", MessageBoxButtons.OKCancel, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) == DialogResult.OK)
                    {
                        file.CopyTo(path, true);

                    }
                    else
                    {
                        addFile = false;
                    };


                };


                file = new FileInfo(path);
                #endregion

                if (addFile == true)
                {
                    // Hassan T20100331.0004 2010-05-02 [End]

                    AriaObject currentObject = new AriaObject();

                    InterOper interOper = new InterOper();
                    currentObject.ObjectId = interOper.GetNewObjectKey(InterOperability);
                    currentObject.File = file;
                    currentObject.Name = currentObject.File.Name.Replace(currentObject.File.Extension, "");

                    _objects.AddObject(currentObject);

                    _objects.ActiveObjectIndex = _objects.Count - 1;



                    interOper.AddNewObject(InterOperability, currentObject.ObjectId, currentObject.Name, currentObject.Notes, currentObject.File.FullName, currentObject.SftFormatFileName);

                    interOper.AddNewObjectLink(InterOperability, _objectType, _objectKey, currentObject.ObjectId);
                    // Hassan T20100331.0004 2010-05-02 [Begin]
                };
                // Hassan T20100331.0004 2010-05-02 [End]
            }

            // MOH 21-7-2009 Save object to DBFS\company\objects folder End
        }

        private void printToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ((AriaThumbnailControl)objectsContainer.Controls[_objects.ActiveObjectIndex]).Print();
        }

        private void propertiesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ShowTaskPane = true;
            taskPane.SelectedIndex = 0;
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaObjectBrowserMain_FormClosing(null, null);
            Dispose();
        }
        #endregion

        #region Edit Menu
        private void cutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaClipboardObject ariaObject = new AriaClipboardObject();
            ariaObject.ObjectId = _objects[_objects.ActiveObjectIndex].ObjectId;
            ariaObject.Name = _objects[_objects.ActiveObjectIndex].Name;
            ariaObject.Notes = _objects[_objects.ActiveObjectIndex].Notes;
            ariaObject.IsDefault = _objects[_objects.ActiveObjectIndex].IsDefault;
            ariaObject.File = _objects[_objects.ActiveObjectIndex].File.FullName;

            XmlSerializer xmlSerializer = new XmlSerializer(typeof(AriaClipboardObject));

            DataObject dataObject = new DataObject();
            TextWriter textWriter = new StringWriter();
            xmlSerializer.Serialize(textWriter, ariaObject);
            dataObject.SetText(textWriter.ToString());
            dataObject.SetImage(((AriaThumbnailControl)objectsContainer.Controls[_objects.ActiveObjectIndex]).iconPictureBox.Image);
            Clipboard.SetDataObject(dataObject, true);

            deleteToolStripMenuItem_Click(null, null);
        }

        private void copyToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaClipboardObject ariaObject = new AriaClipboardObject();
            ariaObject.ObjectId = _objects[_objects.ActiveObjectIndex].ObjectId;
            ariaObject.Name = _objects[_objects.ActiveObjectIndex].Name;
            ariaObject.Notes = _objects[_objects.ActiveObjectIndex].Notes;
            ariaObject.IsDefault = _objects[_objects.ActiveObjectIndex].IsDefault;
            ariaObject.File = _objects[_objects.ActiveObjectIndex].File.FullName;

            XmlSerializer xmlSerializer = new XmlSerializer(typeof(AriaClipboardObject));

            DataObject dataObject = new DataObject();
            TextWriter textWriter = new StringWriter();
            xmlSerializer.Serialize(textWriter, ariaObject);
            dataObject.SetText(textWriter.ToString());
            dataObject.SetImage(((AriaThumbnailControl)objectsContainer.Controls[_objects.ActiveObjectIndex]).iconPictureBox.Image);
            Clipboard.SetDataObject(dataObject, true);
        }

        private void pasteToolStripMenuItem_Click(object sender, EventArgs e)
        {
            XmlSerializer xmlSerializer = new XmlSerializer(typeof(AriaClipboardObject));
            TextReader textRead = new StringReader(Clipboard.GetText());
            AriaClipboardObject ariaObject = (AriaClipboardObject)xmlSerializer.Deserialize(textRead);

            InterOper interOper = new InterOper();
            AriaObject currentObject = new AriaObject();
            currentObject.ObjectId = interOper.GetNewObjectKey(InterOperability);
            currentObject.File = new FileInfo(ariaObject.File);
            currentObject.Name = ariaObject.Name;
            currentObject.Notes = ariaObject.Notes;

            _objects.AddObject(currentObject);

            _objects.ActiveObjectIndex = _objects.Count - 1;

            interOper.AddNewObject(InterOperability, currentObject.ObjectId, currentObject.Name, currentObject.Notes, currentObject.File.FullName, currentObject.SftFormatFileName);
            interOper.AddNewObjectLink(InterOperability, _objectType, _objectKey, currentObject.ObjectId);
        }

        private void editToolStripMenuItem_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start(_objects[_objects.ActiveObjectIndex].File.FullName);
        }

        private void deleteToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (MessageBox.Show("Do you want to delete this file?", "Confirm Delete", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) == DialogResult.Yes)
            {
                InterOper interOper = new InterOper();
                interOper.DeleteObject(InterOperability, _objects[_objects.ActiveObjectIndex].ObjectId);
                interOper.DeleteObjectLink(InterOperability, _objectType, _objectKey, _objects[_objects.ActiveObjectIndex].ObjectId);

                // MOH 21-7-2009 Delete file from objects folder Start
                try
                {
                    _objects[_objects.ActiveObjectIndex].File.Delete();
                }
                catch { }
                _objects.DeleteObject(_objects.ActiveObjectIndex);
                // MOH 21-7-2009 Delete file from objects folder End
            }
        }

        private void renameToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ShowTaskPane = true;
            taskPane.SelectedIndex = 1;
        }

        private void changeNotesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ShowTaskPane = true;
            taskPane.SelectedIndex = 2;
        }

        private void markAsDefaultToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _objects.DefaultObjectIndex = _objects.ActiveObjectIndex;

            InterOper interOper = new InterOper();
            interOper.SetDefaultObject(InterOperability, _objectType, _objectKey, _objects[_objects.ActiveObjectIndex].ObjectId);

        }
        #endregion

        #region View Menu
        private void thumbnailsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ViewType = AriaViewType.Thumbnail;
        }

        private void filmstripToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ViewType = AriaViewType.Filmstrip;
        }

        private void singleFileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ViewType = AriaViewType.SingleFile;
        }

        private void taskPaneToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ShowTaskPane = !_settings.ShowTaskPane;
        }

        private void sortByFileNameToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.SortType = AriaSortType.FileName;
        }

        private void sortByDateToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.SortType = AriaSortType.Date;
        }

        private void sortByFileTypeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.SortType = AriaSortType.FileType;
        }

        private void sortByFileSizeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.SortType = AriaSortType.FileSize;
        }

        private void showFileNamesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.ShowFileNames = !_settings.ShowFileNames;
        }

        private void refreshToolStripMenuItem_Click(object sender, EventArgs e)
        {
            _settings.FireRefreshAll();
        }
        #endregion

        #region Help Menu
        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
        }
        #endregion

        #region Standard Toolbar
        private void printToolStripButton_Click(object sender, EventArgs e)
        {
            printToolStripMenuItem_Click(null, null);
        }

        private void markAsDefaultToolStripButton_Click(object sender, EventArgs e)
        {
            markAsDefaultToolStripMenuItem_Click(null, null);
        }

        private void cutToolStripButton_Click(object sender, EventArgs e)
        {
            cutToolStripMenuItem_Click(null, null);
        }

        private void copyToolStripButton_Click(object sender, EventArgs e)
        {
            copyToolStripMenuItem_Click(null, null);
        }

        private void pasteToolStripButton_Click(object sender, EventArgs e)
        {
            pasteToolStripMenuItem_Click(null, null);
        }

        private void editToolStripButton_Click(object sender, EventArgs e)
        {
            System.Diagnostics.Process.Start(_objects[_objects.ActiveObjectIndex].File.FullName);
        }

        private void deleteToolStripButton_Click(object sender, EventArgs e)
        {
            deleteToolStripMenuItem_Click(null, null);
        }

        private void zoomToolStripComboBox_TextChanged(object sender, EventArgs e)
        {
            if (zoomToolStripComboBox.Text != _settings.Zoom.ToString() + "%")
            {
                int oldValue = _settings.Zoom;

                int newValue;
                int.TryParse(zoomToolStripComboBox.Text.Replace("%", ""), out newValue);

                if (newValue >= 12 && newValue <= 800)
                {
                    _settings.Zoom = newValue;
                }
                else
                {
                    zoomToolStripComboBox.Text = oldValue.ToString() + "%";
                }
            }
        }
        #endregion

        #region View Toolbar
        private void thumbnailsToolStripButton_Click(object sender, EventArgs e)
        {
            _settings.ViewType = AriaViewType.Thumbnail;
        }

        private void filmstripToolStripButton_Click(object sender, EventArgs e)
        {
            _settings.ViewType = AriaViewType.Filmstrip;
        }

        private void singleFileToolStripButton_Click(object sender, EventArgs e)
        {
            _settings.ViewType = AriaViewType.SingleFile;
        }
        #endregion

        #region Statusbar
        private void moveNextButton_Click(object sender, EventArgs e)
        {
            _objects.ActiveObjectIndex++;
        }

        private void movePreviousButton_Click(object sender, EventArgs e)
        {
            _objects.ActiveObjectIndex--;
        }

        private void trackBar_Scroll(object sender, EventArgs e)
        {
            if (_settings.Zoom != trackBar.Value)
            {
                _settings.Zoom = trackBar.Value;
            }
        }
        #endregion

        #region Task Pane
        private void taskPane_TaskPaneCloseClick(object sender, EventArgs e)
        {
            _settings.ShowTaskPane = false;
        }

        #region Rename Task Pane
        private void renameFile_Click(object sender, EventArgs e)
        {
            renameFileNameLabel.Text = renameFileName.Text;
            changeNotesFileNameLabel.Text = renameFileName.Text;

            _objects[_objects.ActiveObjectIndex].Name = renameFileName.Text;

            _objects_ActiveObjectChanged();

            InterOper interOper = new InterOper();
            interOper.UpdateObjectName(InterOperability, _objects[_objects.ActiveObjectIndex].ObjectId, _objects[_objects.ActiveObjectIndex].Name);
        }

        private void renameFileName_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '\r')
            {
                renameFile_Click(null, null);
            }
        }
        #endregion

        #region Properties Task Pane
        #endregion

        #region Change Notes Task Pane
        private void changeNotes_Click(object sender, EventArgs e)
        {
            _objects[_objects.ActiveObjectIndex].Notes = changeNotesNotes.Text;

            _objects_ActiveObjectChanged();

            InterOper interOper = new InterOper();
            interOper.UpdateObjectNotes(InterOperability, _objects[_objects.ActiveObjectIndex].ObjectId, _objects[_objects.ActiveObjectIndex].Notes);
        }

        private void changeNotesNotes_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '\r')
            {
                changeNotes_Click(null, null);
            }
        }
        #endregion

        private void AriaObjectBrowserMain_Load(object sender, EventArgs e)
        {
        }
        #endregion

        private void objectsContainer_Enter(object sender, EventArgs e)
        {
            //MessageBox.Show("a1");
        }

        private void singleFileControl_Enter(object sender, EventArgs e)
        {
            //MessageBox.Show("a2");
        }

        #region SingleFileView
        #endregion
        //Moh 21-7-2009 Add property to hold objects save path start
        internal string getObjectsPath()
        {
            string connection = Connectionstring;
            System.Data.Odbc.OdbcConnection con = new System.Data.Odbc.OdbcConnection(connection);
            con.Open();
            string path = con.Database;
            con.Close();
            path = path.Trim().EndsWith("\\") ? path : path + "\\";
            path += "Objects\\";
            if (!Directory.Exists(path)) Directory.CreateDirectory(path);
            return path;
        }
        //Moh 21-7-2009 Add property to hold objects save path End
    }
}