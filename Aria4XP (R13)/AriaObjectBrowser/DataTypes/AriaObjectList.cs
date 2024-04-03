using System;
using System.Collections.Generic;
using System.Text;
using AriaObjectBrowser.Enums;


namespace AriaObjectBrowser.DataTypes
{
    public delegate void AriaAddObjectChangedEventHandler(int index);
    public delegate void AriaDeleteObjectChangedEventHandler(int index);
    public delegate void AriaActiveObjectChangedEventHandler();
    public delegate void AriaDefaultObjectChangedEventHandler();
    
    public class AriaObjectList : List<AriaObject>
    {
        public event AriaAddObjectChangedEventHandler ObjectAdded;
        public event AriaDeleteObjectChangedEventHandler ObjectDeleted;
        public event AriaActiveObjectChangedEventHandler ActiveObjectChanged;
        public event AriaDefaultObjectChangedEventHandler DefaultObjectChanged;

        private AriaSortType _currentSortType = AriaSortType.NotSet;

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

        public AriaObjectList()
        {
        }

        void AriaSettings_Changed()
        {
            if (_currentSortType != _settings.SortType) Sort(_settings.SortType);

        }

        public class AriaSortByFileName : Comparer<AriaObject>
        {
            public override int Compare(AriaObject x, AriaObject y)
            {
                return x.File.Name.CompareTo(y.File.Name);
            }
        }

        public class AriaSortByDate : Comparer<AriaObject>
        {
            public override int Compare(AriaObject x, AriaObject y)
            {
                return x.File.LastWriteTime.CompareTo(y.File.LastWriteTime);
            }
        }

        public class AriaSortByFileType : Comparer<AriaObject>
        {
            public override int Compare(AriaObject x, AriaObject y)
            {
                return x.File.Extension.CompareTo(y.File.Extension);
            }
        }

        public class AriaSortByFileSize : Comparer<AriaObject>
        {
            public override int Compare(AriaObject x, AriaObject y)
            {
                return x.File.Length.CompareTo(y.File.Length);
            }
        }

        public void AddObject(AriaObject item)
        {
            Add(item);

            if (ObjectAdded != null) ObjectAdded(Count - 1);

            if (Count == 1) ActiveObjectIndex = 0;
        }

        public void DeleteObject(int index)
        {
            RemoveAt(index);

            if (Count == 0)
            {
                ActiveObjectIndex = -1;
            }
            else if (index == 0)
            {
                ActiveObjectIndex = 0;
            }
            else if (index == Count)
            {
                ActiveObjectIndex = Count - 1;
            }
            else
            {
                ActiveObjectIndex = index - 1;
            }
            

            DefaultObjectIndex = -1;
            for (int i = 0; i < Count; i++)
            {
                if (this[i].IsDefault)
                {
                    DefaultObjectIndex = i;
                    break;
                }
            }

            if (ObjectDeleted != null) ObjectDeleted(index);
        }

        public void Sort(AriaSortType sortType)
        {
            switch (sortType)
            {
                case AriaSortType.FileName:
                    Sort(new AriaSortByFileName());
                    break;

                case AriaSortType.Date:
                    Sort(new AriaSortByDate());
                    break;

                case AriaSortType.FileType:
                    Sort(new AriaSortByFileType());
                    break;

                case AriaSortType.FileSize:
                    Sort(new AriaSortByFileSize());
                    break;
            }

            _currentSortType = _settings.SortType;

            for (int i = 0; i < Count; i++)
            {
                if (this[i].IsActive) ActiveObjectIndex = i;
                if (this[i].IsDefault) DefaultObjectIndex = i;
            }
        }

        private int _defaultObjectIndex = -1;
        public int DefaultObjectIndex
        {
            get { return _defaultObjectIndex; }
            set 
            {
                if (Count > _defaultObjectIndex && _defaultObjectIndex >= 0)
                {
                    this[_defaultObjectIndex].IsDefault = false;
                }

                _defaultObjectIndex = value;

                if (Count > _defaultObjectIndex && _defaultObjectIndex >= 0)
                {
                    this[_defaultObjectIndex].IsDefault = true;
                }

                if (DefaultObjectChanged != null) DefaultObjectChanged();
            }
        }

        private int _activeObjectIndex = -1;
        public int ActiveObjectIndex
        {
            get { return _activeObjectIndex; }
            set 
            {
                if (Count > _activeObjectIndex && _activeObjectIndex >= 0)
                {
                    this[_activeObjectIndex].IsActive = false;
                }

                _activeObjectIndex = value;

                if (_activeObjectIndex >= 0)
                {
                    this[_activeObjectIndex].IsActive = true;
                }

                if (ActiveObjectChanged != null) ActiveObjectChanged(); 
            }
        }
    }
}
