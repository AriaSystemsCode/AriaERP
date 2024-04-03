using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using AriaObjectBrowser.DataTypes;
using AriaObjectBrowser.Enums;


namespace AriaObjectBrowser.UserControls
{
    public partial class AriaObjectsContainer : UserControl
    {
        private AriaSettings _settings;
        public AriaSettings Settings
        {
            get { return _settings; }
            set 
            {
                _settings = value;
                if (_settings == null) return;

                _settings.Changed += new AriaSettingChangedEventHandler(AriaSettings_Changed);
                _settings.RefreshAll += new AriaSettingRefreshAllHandler(AriaSettings_RefreshAll);
            }
        }

        private object _ariaApplication;
        public object AriaApplication
        {
            get { return _ariaApplication; }
            set { _ariaApplication = value; }
        }

        private AriaObjectList _objects;
        public AriaObjectList AriaObjects
        {
            get { return _objects; }
            set 
            {
                _objects = value;

                if (_objects != null)
                {
                    _objects.ObjectAdded += new AriaAddObjectChangedEventHandler(_ariaObjects_ObjectAdded);
                    _objects.ObjectDeleted += new AriaDeleteObjectChangedEventHandler(_ariaObjects_ObjectDeleted);
                    _objects.ActiveObjectChanged += new AriaActiveObjectChangedEventHandler(_ariaObjects_ActiveObjectChanged);
                    _objects.DefaultObjectChanged += new AriaDefaultObjectChangedEventHandler(_ariaObjects_DefaultObjectChanged);
                }
            }
        }

        public AriaObjectsContainer()
        {
            InitializeComponent();
            HorizontalScroll.Enabled = false;

            Resize += new EventHandler(AriaObjectsContainer_Resize);
        }

        private void AddFilmstripObject(int index)
        {
            SuspendLayout(); 
            
            int currentFileTop = 0;
            for (int i = 0; i < Controls.Count; i++)
            {
                currentFileTop += Controls[i].Height + 4;
            }

            Controls.Add(new AriaThumbnailControl(_ariaApplication, _settings, _objects, index));
            Controls[Controls.Count - 1].Top = currentFileTop + 4;
            Controls[Controls.Count - 1].Left = 2;
            Controls[Controls.Count - 1].Width = Width - 8;

            Controls[Controls.Count - 1].Visible = true;

            Controls[Controls.Count - 1].Focus();

            if (Controls[Controls.Count - 1].Top + Controls[Controls.Count - 1].Height > Height)
            {
                for (int i = 0; i < Controls.Count; i++)
                {
                    Controls[i].Width = Width - 20;
                }
            }

            ResumeLayout();
        }

        int _currentObjectCol = 0;
        int _currentObjectRow = 0;
        private void AddThumbnailObject(int index)
        {
            SuspendLayout();

            Size thumbnailSize = new Size(Convert.ToInt32(160.0 * Convert.ToDouble(_settings.Zoom) / 100.00),
                                          Convert.ToInt32(200.0 * Convert.ToDouble(_settings.Zoom) / 100.00));
            
            int colCount = (Width - 20) / (thumbnailSize.Width + 8);

            if (colCount == 0) colCount = 1;

            Controls.Add(new AriaThumbnailControl(_ariaApplication, _settings, _objects, index));
            Controls[Controls.Count - 1].Location = new Point(_currentObjectCol * (8 + thumbnailSize.Width), _currentObjectRow * (8 + thumbnailSize.Height));
            Controls[Controls.Count - 1].Size = thumbnailSize;

            Controls[Controls.Count - 1].Visible = true;

            _currentObjectCol++;

            if (_currentObjectCol == colCount)
            {
                _currentObjectCol = 0;
                _currentObjectRow++;
            }

            ResumeLayout();
        }

        void _ariaObjects_ObjectAdded(int index)
        {
            switch (_settings.ViewType)
            {
                case AriaViewType.Filmstrip:
                    AddFilmstripObject(index);
                    break;

                case AriaViewType.SingleFile:
                    AddFilmstripObject(index);
                    break;

                case AriaViewType.Thumbnail:
                    AddThumbnailObject(index);
                    break;
            }
        }

        private void RefreshAllFilmstripObjects()
        {
            Parent.SuspendLayout();

            for (int i = Controls.Count - 1; i > -1; i--)
            {
                Controls.RemoveAt(i);
            }

            if (_objects != null)
            {
                for (int i = 0; i < _objects.Count; i++)
                {
                    _ariaObjects_ObjectAdded(i);
                }
            }

            Parent.ResumeLayout();
        }

        private void RefreshAllThumbnailObjects()
        {
            Parent.SuspendLayout();

            for (int i = Controls.Count - 1; i > -1; i--)
            {
                Controls.RemoveAt(i);
            }

            _currentObjectCol = 0;
            _currentObjectRow = 0;

            if (_objects != null)
            {
                for (int i = 0; i < _objects.Count; i++)
                {
                    _ariaObjects_ObjectAdded(i);
                }
            }

            Parent.ResumeLayout();
        }

        void AriaSettings_RefreshAll()
        {
            _inRefresh = true;
            switch (_settings.ViewType)
            {
                case AriaViewType.Filmstrip:
                    RefreshAllFilmstripObjects();
                    break;

                case AriaViewType.SingleFile:
                    RefreshAllFilmstripObjects();
                    break;

                case AriaViewType.Thumbnail:
                    RefreshAllThumbnailObjects();
                    break;
            }
            _inRefresh = false;
        }


        private void DeleteFilmstripObject(int index)
        {
            Controls.RemoveAt(index);

            for (int i = 0; i < Controls.Count; i++)
            {
                if (i == 0) Controls[0].Top = 4;
                if (i > 0) Controls[i].Top = Controls[i - 1].Top + Controls[i - 1].Height + 4;
            }
        }

        private void DeleteThumbnailObject(int index)
        {
            RefreshAllThumbnailObjects();
        }

        void _ariaObjects_ObjectDeleted(int index)
        {
            switch (_settings.ViewType)
            {
                case AriaViewType.Filmstrip:
                    DeleteFilmstripObject(index);
                    break;

                case AriaViewType.SingleFile:
                    DeleteFilmstripObject(index);
                    break;

                case AriaViewType.Thumbnail:
                    DeleteThumbnailObject(index);
                    break;
            }
        }

        private bool _inRefresh = false;
        void AriaObjectsContainer_Resize(object sender, EventArgs e)
        {
            if (DesignMode == true) return;
            if (_settings == null) return;

            if (_inRefresh) return;

            AriaSettings_RefreshAll();
        }

        void AriaSettings_Changed()
        {
        }

        void _ariaObjects_ActiveObjectChanged()
        {
        }

        void _ariaObjects_DefaultObjectChanged()
        {
        }
    }
}
