using DevExpress.Data.Filtering;
using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Design;
using DevExpress.ExpressApp.Security;
using DevExpress.ExpressApp.Security.Strategy;
using DevExpress.ExpressApp.Utils;
using DevExpress.Utils;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.Design;
using System.Drawing;

namespace Aria5SystemAdmin.Web
{
	/// <summary>
	///     <para>A Security Strategy that maintains users who have a list of associated roles.
	///
	/// </para>
	/// </summary>
	[ToolboxTabName("DX.14.1: XAF Security"), Designer("DevExpress.ExpressApp.Security.Design.SecurityStrategyComplexDesigner, DevExpress.ExpressApp.Security.Design.v14.1, Version=14.1.6.0, Culture=neutral, PublicKeyToken=b88d1754d700e49a", typeof(IDesigner)), DXToolboxItem(true), ToolboxBitmap(typeof(SecurityStrategyComplex), "Resources.Toolbox_SecurityStrategyComplex.ico")]
	public class AriaSecurityStrategy : SecurityStrategy, ISecurityComplex, ISecurityStrategyBase, IRoleTypeProvider
	{
		private Type roleType = typeof(SecuritySystemRole);
		private string newUserRoleName = SecurityStrategy.AdministratorRoleName;
		/// <summary>
		///     <para>Occurs when a user is automatically created.
		/// </para>
		/// </summary>
		public event EventHandler<CustomInitializeNewUserEventArgs> CustomInitializeNewUser;
		/// <summary>
		///     <para>Specifies the type of a security strategy role.
		/// </para>
		/// </summary>
		/// <value>The type of a security strategy role.
		/// </value>
		[TypeConverter(typeof(BusinessClassTypeConverter<ISecuritySystemRole>))]
		public Type RoleType
		{
			get
			{
				return this.roleType;
			}
			set
			{
				this.roleType = value;
			}
		}
		/// <summary>
		///     <para>Specifies the name of role which is assigned to auto-created users.
		/// </para>
		/// </summary>
		/// <value>A string which is the name of the role assigned to auto-created users.
		///
		/// </value>
		[Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
		public string NewUserRoleName
		{
			get
			{
				return this.newUserRoleName;
			}
			set
			{
				this.newUserRoleName = value;
			}
		}
		/// <summary>
		///     <para>Returns a list of persistent types that are used within the current security strategy.
		/// </para>
		/// </summary>
		/// <returns>An <b>IList</b> object that is the list of persistent types that are used within the current security strategy.
		/// </returns>
		public override IList<Type> GetBusinessClasses()
		{
			List<Type> list = new List<Type>(base.GetBusinessClasses());
			if (this.RoleType != null && !list.Contains(this.RoleType))
			{
				list.Add(this.RoleType);
			}
			return list;
		}
		protected override void InitializeNewUserCore(IObjectSpace objectSpace, object user)
		{
			DevExpress.ExpressApp.Utils.Guard.ArgumentNotNull(user, "user");
			DevExpress.ExpressApp.Utils.Guard.ArgumentNotNull(objectSpace, "objectSpace");
			CustomInitializeNewUserEventArgs customInitializeNewUserEventArgs = new CustomInitializeNewUserEventArgs(objectSpace, user);
			if (this.CustomInitializeNewUser != null)
			{
				this.CustomInitializeNewUser(this, customInitializeNewUserEventArgs);
			}
			if (!customInitializeNewUserEventArgs.Handled && typeof(SecuritySystemRole).IsAssignableFrom(this.roleType))
			{
				SecuritySystemRole securitySystemRole = (SecuritySystemRole)objectSpace.FindObject(this.roleType, new BinaryOperator("Name", this.NewUserRoleName));
				if (securitySystemRole == null)
				{
					securitySystemRole = (SecuritySystemRole)objectSpace.CreateObject(this.roleType);
					securitySystemRole.Name = this.NewUserRoleName;
					securitySystemRole.IsAdministrative = true;
				}
				securitySystemRole.Users.Add((SecuritySystemUser)user);
			}
		}
		static AriaSecurityStrategy()
		{
		}
		/// <summary>
		///     <para>Initializes a new instance of the SecurityStrategyComplex class.
		/// </para>
		/// </summary>
		public AriaSecurityStrategy()
		{
		}
		/// <summary>
		///     <para>Initializes a new instance of the SecurityStrategyComplex class with the specified user type, role type, and authentication.
		/// </para>
		/// </summary>
		/// <param name="userType">
		/// 		The type of a security strategy user.
		///
		/// </param>
		/// <param name="roleType">
		/// 		The type of a security strategy role.
		///
		/// </param>
		/// <param name="authentication">
		/// 		An <see cref="T:DevExpress.ExpressApp.Security.AuthenticationBase" /> object which is the authentication used within the security strategy.
		///
		/// </param>
        public AriaSecurityStrategy(Type userType, Type roleType, AuthenticationBase authentication)
            : base(userType, authentication)
		{
			this.roleType = roleType;
		}

        public override bool IsGranted(DevExpress.ExpressApp.Security.IPermissionRequest permissionRequest)
        {
            return true;
        }

        public override IList<bool> IsGranted(IList<IPermissionRequest> permissionRequests)
        {
            return base.IsGranted(permissionRequests);
        }
	}
}




