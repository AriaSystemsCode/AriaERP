// Developer Express Code Central Example:
// How to Use Tree List Editors to Display List Views
// 
// This example demonstrates how to use the TreeListEditor and
// CategorizedListEditor, which are supplied with the
// TreeListEditorsWindowsFormsModule. The following techniques are used:
// 
// 1) A
// List View that represents objects of the type that implements the ITreeNode
// interface is automatically presented by the TreeListEditor.
// See the Category
// class and its descendants: ProjectGroup, Project and ProjectArea.
// For details,
// refer to the "TreeList Editors Module Overview" and "Implement the ITreeNode
// Interface" topics in the Concepts | Extra Modules | TreeList Editors Module
// section of the XAF documentation.
// 
// 2) A List View that represents objects of
// the type that implements the ICategorizedItem interface is automatically
// presented by the CategorizedListEditor.
// See the Issue class that is related to
// the CategoryWithIssues class by the Many-to-One relationship.
// For details,
// refer to the "TreeList Editors Module Overview" and "Implement the
// ICategorizedItem Interface" topics in the Concepts | Extra Modules | TreeList
// Editors Module section of the XAF documentation.
// 
// 3) A List View that
// represents objects of the HCategory type, which is supplied with the Business
// Class Library, is automatically presented by the TreeListEditor.
// The HCategory
// class is added to the application's business model via the Module Designer of
// the common module project.
// For details, refer to the TreeList Editors Module
// Overview (ms-help://DevExpress.Xaf/CustomDocument2836.htm) and Use the Built-in
// HCategory Class (ms-help://DevExpress.Xaf/CustomDocument2839.htm) topics in the
// XAF documentation.
// 
// You can find sample updates and versions for different programming languages here:
// http://www.devexpress.com/example=E1125

using System;

using DevExpress.Xpo;

using DevExpress.ExpressApp;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Persistent.Validation;
using DevExpress.Persistent.Base.General;
using System.ComponentModel;
using DevExpress.Xpo.DB;


namespace Aria5SystemAdmin.Module
{
    
    

    [NavigationItem]
    public abstract class CategoryWithIssues : BaseObject1, ITreeNode
    {
        [Association("CategoryWithIssues-Issues")]
        public XPCollection<Issue> Issues
        {
            get
            {
                return GetCollection<Issue>("Issues");
            }
        }
        private XPCollection<Issue> allIssues;
        public XPCollection<Issue> AllIssues
        {
            get
            {
                if (allIssues == null)
                {
                    allIssues = new XPCollection<Issue>(Session, false);
                    CollectIssuesRecursive(this, allIssues);
                    allIssues.BindingBehavior = CollectionBindingBehavior.AllowNone;
                }
                return allIssues;
            }
        }
        private void CollectIssuesRecursive(CategoryWithIssues issueCategory, XPCollection<Issue> target)
        {
            target.AddRange(issueCategory.Issues);
            
            foreach (CategoryWithIssues childCategory in issueCategory.Children)
            {
                CollectIssuesRecursive(childCategory, target);
            }
        }

       
        
        private string name;
        protected abstract ITreeNode Parent
        {
            get;
        }

        protected abstract IBindingList Children
        {
            get;
        }

        public CategoryWithIssues(Session session) : base(session) { }
        public string Name
        {
            get
            {
                return name;
            }
            set
            {
                SetPropertyValue("Name", ref name, value);
            }
        }

        #region ITreeNode
        IBindingList ITreeNode.Children
        {
            get
            {
                return Children;
            }
        }
        string ITreeNode.Name
        {
            get
            {
                return Name;
            }
        }
        ITreeNode ITreeNode.Parent
        {
            get
            {
                return Parent;
            }
        }
        #endregion
    }
    public class ProjectGroupWithIssues : CategoryWithIssues
    {
        protected override ITreeNode Parent
        {
            get
            {    
                return null;
            }
        }
        protected override IBindingList Children
        {
            get
            {
                return ProjectsWithIssues;
            }
        }
        public ProjectGroupWithIssues(Session session) : base(session) { }
        public ProjectGroupWithIssues(Session session, string name)
            : base(session)
        {
            this.Name = name;
        }
        [Association("ProjectGroupWithIssues-ProjectsWithIssues"), Aggregated]
        public XPCollection<ProjectWithIssues> ProjectsWithIssues
        {
            get
            {
                return GetCollection<ProjectWithIssues>("ProjectsWithIssues");
            }
        }
    }

    public class ProjectWithIssues : CategoryWithIssues
    {
        private ProjectGroupWithIssues projectGroupWithIssues;
        protected override ITreeNode Parent
        {
            get
            {
                return projectGroupWithIssues;
            }
        }
        protected override IBindingList Children
        {
            get
            {
                return ProjectAreasWithIssues;
            }
        }
        public ProjectWithIssues(Session session) : base(session) { }
        public ProjectWithIssues(Session session, string name)
            : base(session)
        {
            this.Name = name;
        }
        [Association("ProjectGroupWithIssues-ProjectsWithIssues")]
        public ProjectGroupWithIssues ProjectGroupWithIssues
        {
            get
            {
                return projectGroupWithIssues;
            }
            set
            {
                SetPropertyValue("ProjectGroupWithIssues", ref projectGroupWithIssues, value);
            }
        }
        [Association("ProjectWithIssues-ProjectAreasWithIssues"), Aggregated]
        public XPCollection<ProjectAreaWithIssues> ProjectAreasWithIssues
        {
            get
            {
                return GetCollection<ProjectAreaWithIssues>("ProjectAreasWithIssues");
            }
        }
    }

    public class ProjectAreaWithIssues : CategoryWithIssues
    {
        private ProjectWithIssues projectWithIssues;
        protected override ITreeNode Parent
        {
            get
            {
                return projectWithIssues;
            }
        }
        protected override IBindingList Children
        {
            get
            {
                return new BindingList<object>();
            }
        }
        public ProjectAreaWithIssues(Session session) : base(session) { }
        public ProjectAreaWithIssues(Session session, string name)
            : base(session)
        {
            this.Name = name;
        }
        [Association("ProjectWithIssues-ProjectAreasWithIssues")]
        public ProjectWithIssues ProjectWithIssues
        {
            get
            {
                return projectWithIssues;
            }
            set
            {
                SetPropertyValue("ProjectWithIssues", ref projectWithIssues, value);
            }
        }
    }



    [DefaultClassOptions]
    public class Issue : BaseObject1, ICategorizedItem
    {
        private CategoryWithIssues categoryWithIssues;
        private string subject;
        private string description;
        public Issue(Session session) : base(session) { }
        public Issue(Session session, string subject)
            : base(session)
        {
            this.subject = subject;
        }
        [Association("CategoryWithIssues-Issues")]
        public CategoryWithIssues Category
        { 
            get
            {
                return categoryWithIssues;
            }
            set
            {
                SetPropertyValue("Category", ref categoryWithIssues, value);
            }
        }
        public string Subject
        {
            get
            {
                return subject;
            }
            set
            {
                SetPropertyValue("Subject", ref subject, value);
            }
        }
        public string Description
        {
            get
            {
                return description;
            }
            set
            {
                SetPropertyValue("Description", ref description, value);
            }
        }
        ITreeNode ICategorizedItem.Category
        {
            get
            {
                return Category;
            }
            set
            {
                Category = (CategoryWithIssues)value;
            }
        }
    }


}
