using System;
using System.Collections.Generic;
using System.Text;
using AriaObjectBrowser.Enums;


namespace AriaObjectBrowser.DataTypes
{
    public delegate void AriaSettingChangedEventHandler();
    public delegate void AriaSettingRefreshAllHandler();

    public class AriaSettings
    {
        public event AriaSettingRefreshAllHandler RefreshAll;

        public event AriaSettingChangedEventHandler Changed;

        private AriaViewType _viewType = AriaViewType.Thumbnail;
        public AriaViewType ViewType
        {
            get { return _viewType; }
            set { _viewType = value; if (Changed != null) Changed(); FireRefreshAll();  }
        }

        private AriaSortType _sortType = AriaSortType.FileName;
        public AriaSortType SortType
        {
            get { return _sortType; }
            set { _sortType = value; if (Changed != null) Changed(); FireRefreshAll();  }
        }

        private bool _showTaskPane = true;
        public bool ShowTaskPane
        {
            get { return _showTaskPane; }
            set { _showTaskPane = value; if (Changed != null) Changed(); }
        }

        private bool _showFileNames = true;
        public bool ShowFileNames
        {
            get { return _showFileNames; }
            set { _showFileNames = value; if (Changed != null) Changed(); }
        }

        private int _zoom = 100;
        public int Zoom
        {
            get { return _zoom; }
            set { _zoom = value; if (Changed != null) Changed(); if (_viewType == AriaViewType.Thumbnail && RefreshAll != null) RefreshAll(); }
        }

        public void FireRefreshAll()
        {
            if (RefreshAll != null) RefreshAll();
        }
    }
}
