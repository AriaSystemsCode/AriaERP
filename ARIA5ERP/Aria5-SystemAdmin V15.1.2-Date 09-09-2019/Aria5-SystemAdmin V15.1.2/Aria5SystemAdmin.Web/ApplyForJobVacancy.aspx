<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="ApplyForJobVacancy.aspx.cs" Inherits="Clioprototype.Web.ApplyForJobVacancy" %>
<script src="Scripts/jquery-1.10.2.js"></script>
<script src="Scripts/jquery-1.10.2.min.js"></script>
<script src="Scripts/jquery.validate.js"></script>
<head>
<link href="CSS/bootstrap.css" rel="stylesheet" />
<link href="CSS/bootstrap.min.css" rel="stylesheet" />
<script language="JavaScript">
<!--
    function ListBoxValid(sender, args) {
         var options = document.getElementById("<%=ListBox1.ClientID%>").options;
        if (options.length > 0) {
            args.IsValid = true;
        }
        else {
            args.IsValid = false;
        }
    }
    function ListBox2Valid(sender, args) {
        var options = document.getElementById("<%=LB_educations.ClientID%>").options;
        if (options.length > 0) {
            args.IsValid = true;
        }
        else {
            args.IsValid = false;
        }
    }
    var validFilesTypes = ["Pdf"];
    function ValidateFile() {
        alert("Hi")
        var file = document.getElementById("<%=FileUpload2.ClientID%>");
        var path = file.value;
        var ext = path.substring(path.lastIndexOf(".") + 1, path.length).toLowerCase();
        alert(ext);
        var isValidFile = false;
        for (var i = 0; i < validFilesTypes.length; i++) {
            if (ext == validFilesTypes[i]) {
                isValidFile = true;
                break;
            }
        }
        alert(isValidFile);
        return isValidFile;
    }
    // -->
</script>    
<%--<script type="text/javascript">
    $(function ()
    {
        $("Form [Name = 'JobVacancy']").validate(
            {
            rules:
                {
                Can_fName: 'Can_fName',
              },
           messages:
              {
                    Can_fName:"First Name is required"
              },
            submitHandler: function (form) {
                form.submit();
            }
            }
        );

    });
</script>--%>
    <style type="text/css">
        .auto-style1 {
            font-size: large;
            text-align: center;
        }
        .auto-style2 {
            text-align: justify;
        }
    </style>
</head>
        
    <div style="width:70%; margin:0 auto; margin-top:10PX; background-color:ghostwhite;">
        <form id="form2" name="JobVacancy" runat="server" style="align-items:center">
    <img src="CSS/Aria%20logo.png" style="margin-left:25PX; margin-top:10PX; width: 203px;"; />
    <asp:Panel ID="Panel1" runat="server" Height="188px" Visible="False" Width="669px">
            <div class="auto-style2">             
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <strong><span class="auto-style1">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;Application submitted succefully</span></strong></div>
    </asp:Panel>
    
    <asp:Panel ID="Panel2"  runat="server" Height="414px" Width="668px">

        &nbsp;
  <div class="form-group">  
        &nbsp; Apply For:&nbsp;  
        <asp:Label ID="LbPositionName" runat="server" Text="Position"></asp:Label>
        </div>
      <div class="form-group">  
    FirstName *:
    <asp:TextBox ID="Can_fName" name="Can_fName" class="form-control" runat="Server" style="width:50%" ValidateRequestMode="Enabled"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ControlToValidate="Can_fName" Display="Dynamic" ErrorMessage="Please add your first name" ForeColor="Red" SetFocusOnError="True"></asp:RequiredFieldValidator>
    </div>
    <div class="form-group">
    LastName *:
    <asp:TextBox ID="Can_lName" class="form-control" runat="Server" style="width:50%"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ControlToValidate="Can_lName" ErrorMessage="please add your last name"  ForeColor="Red" SetFocusOnError="True"></asp:RequiredFieldValidator>
     </div>
     <div class="form-group">
        Gender:
    <asp:DropDownList ID="DDl_Gender" class="form-control" style="width:50%" runat="server">
            <asp:ListItem>Female</asp:ListItem>
            <asp:ListItem>Male</asp:ListItem>
        </asp:DropDownList>
    </div>
    <div class="form-group">
    Mobile Number *:
    <asp:TextBox  ID="Can_Mob" class="form-control" runat="Server" TextMode="Phone" style="width:50%"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ControlToValidate="Can_Mob" ErrorMessage="Please add your phone number" ForeColor="Red"></asp:RequiredFieldValidator>
        <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" ControlToValidate="Can_Mob" ErrorMessage="Please enter a valid phone number " ForeColor="Red" ValidationExpression="0\d\d\d\d\d\d\d\d\d\d"></asp:RegularExpressionValidator>
    </div>
    <div class="form-group">
    E-Mail *:
    <asp:TextBox ID="Can_email" class="form-control" runat="Server" TextMode="eMail" style="width:50%"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator4" runat="server" ControlToValidate="Can_email" ErrorMessage="Please add your E-mail address" ForeColor="Red"></asp:RequiredFieldValidator>
    </div>
    <div class="form-group">
    Nationality *:
    <asp:TextBox  ID="Can_Nation" class="form-control" runat="Server" style="width:50%"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator5" runat="server" ControlToValidate="Can_Nation" ErrorMessage="Please add your nationality" ForeColor="Red"></asp:RequiredFieldValidator>
    </div>
    <div class="form-group">
    Military Status:
        <br />
    <asp:DropDownList ID="DDl_militarystatus" class="form-control" style="width:50%" runat="server">
            <asp:ListItem>Complete</asp:ListItem>
            <asp:ListItem>Willserve</asp:ListItem>
            <asp:ListItem>Exampted</asp:ListItem>
            <asp:ListItem>Postponed</asp:ListItem>
        </asp:DropDownList>
    </div>
    <div class="form-group">
    Date Of Birth *:<br />
    <asp:TextBox  ID="Can_BDate" class="form-control" runat="Server"  TextMode="Date" style="width:50%"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator6" runat="server" ControlToValidate="Can_BDate" ForeColor="Red" ErrorMessage="please add your birthdate"></asp:RequiredFieldValidator>
        <h2 style="font:800">Experience:</h2>
        From:
        <asp:TextBox   ID="Exp_from" runat="server" class="form-control" TextMode="Date" Width="161px"></asp:TextBox>
         <asp:RequiredFieldValidator ID="RequiredFieldValidator15" runat="server" ControlToValidate="Exp_from" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="ExperienceGroup"></asp:RequiredFieldValidator>
        <br />
         To:
        <asp:TextBox  ID="Exp_to" runat="server" class="form-control"  TextMode="Date" Width="162px"></asp:TextBox >
         <asp:RequiredFieldValidator ID="RequiredFieldValidator16" runat="server" ControlToValidate="Exp_to" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="ExperienceGroup"></asp:RequiredFieldValidator>
        <br />
         CompanyName:
        <asp:TextBox  ID="Exp_com" runat="server" class="form-control" Width="162px"></asp:TextBox >
        <asp:RequiredFieldValidator ID="RequiredFieldValidator14" runat="server" ControlToValidate="Exp_com" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="ExperienceGroup"></asp:RequiredFieldValidator>
        <br />
        Job Title:
        <asp:TextBox  ID="Exp_jpos" runat="server"  class="form-control" Width="159px"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator13" runat="server" ControlToValidate="Exp_jpos" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="ExperienceGroup"></asp:RequiredFieldValidator>
        <br />
        Reason Of Leaving :
        <asp:TextBox  ID="Exp_reasons" runat="server"  class="form-control" TextMode="MultiLine" Width="209px"></asp:TextBox>
            <br />
        <%--<Button type="button"  class="btn btn-primary" ID="Button1" runat="server"> Add another Experience </Button>--%>
        <asp:Button class="btn btn-primary" ID="Button1" text="Add Experience" runat="Server" ValidationGroup="ExperienceGroup" OnClick="Button1_Click"/>
        <br />
        <asp:listbox ID="ListBox1" class="form-control" runat="server" Width="296px" OnInit="ListBox1_Init" OnSelectedIndexChanged="ListBox1_SelectedIndexChanged"></asp:listbox>
      <!--<asp:CustomValidator ID="CustomValidator2" runat="server" ClientValidationFunction="ListBoxValid" ErrorMessage="Please Add at least one Experience" ControlToValidate="ListBox1" ForeColor="Red" ValidateEmptyText="True"></asp:CustomValidator>-->
        </div>
         <div class="form-group">
       <h3 style="font:800"> Education: </h3>
        Degree: 
        <asp:TextBox ID="Edu_Degree" class="form-control" runat="Server" style="width:25%"></asp:TextBox>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator17" runat="server" ControlToValidate="Edu_Degree" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="EducationGroup"></asp:RequiredFieldValidator>
             <br />
        School/unvirsety name:
         <asp:TextBox ID="Edu_unvirsity" class="form-control" runat="Server" style="width:25%"></asp:TextBox>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator18" runat="server" ControlToValidate="Edu_unvirsity" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="EducationGroup"></asp:RequiredFieldValidator>
             <br />
        Major:                     
        <asp:TextBox ID="Edu_Major" class="form-control" runat="Server" style="width:25%"></asp:TextBox>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator19" runat="server" ControlToValidate="Edu_Major" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="EducationGroup"></asp:RequiredFieldValidator>
             <br />
        Graduation Date :          
        <asp:TextBox ID="Edu_graduationDate" TextMode="Date" class="form-control" runat="Server" style="width:25%"></asp:TextBox>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator20" runat="server" ControlToValidate="Edu_graduationDate" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="EducationGroup"></asp:RequiredFieldValidator>
             <br />
        Grade :                     
       <asp:TextBox ID="Edu_Grade" class="form-control" runat="Server" style="width:25%"></asp:TextBox>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator21" runat="server" ControlToValidate="Edu_Grade" ErrorMessage="This Field is required to add experience " ForeColor="Red" ValidationGroup="EducationGroup"></asp:RequiredFieldValidator>
             <br/>
        <asp:Button class="btn btn-primary" ID="Button2" Text="Add Education" runat="Server" OnClick="Button2_Click" ValidationGroup="EducationGroup"/>
            <br/>
        <asp:ListBox ID="LB_educations" runat="server" Width="298px" OnInit="LB_educations_Init"></asp:ListBox>
             <br />
      <asp:CustomValidator ID="CustomValidator1" runat="server" ClientValidationFunction="ListBox2Valid" ErrorMessage="Please Add at least one Education Record" ControlToValidate="LB_educations" ForeColor="Red" ValidateEmptyText="True"></asp:CustomValidator>

             <%--<asp:CustomValidator ID="CustomValidator1" runat="server" ErrorMessage="CustomValidator"></asp:CustomValidator>--%>
             <br />
        </div>
    <div class="form-group">
        <br />
        Current Salary:<br />
         <asp:TextBox ID="Can_currentSalary" class="form-control" runat="Server" style="width:50%" TextMode="Number"></asp:TextBox>
        <br />                                                   
        Expected Salary *:<br />                                   
         <asp:TextBox ID="Can_expectedsalary" class="form-control" runat="Server" style="width:50%"></asp:TextBox>
        <asp:RequiredFieldValidator ID="RequiredFieldValidator9" runat="server" ControlToValidate="Can_expectedsalary" ForeColor="Red" ErrorMessage="Please add your Expected salary"></asp:RequiredFieldValidator>
        <br />
        <br />                                                   
        Notice Period/Days:<br />                                     
        <asp:TextBox ID="Can_noticeperiod" class="form-control" runat="Server" style="width:50%" TextMode="Number"></asp:TextBox>
        <asp:RangeValidator ID="Noticeperiodvalidator" runat="server" ControlToValidate="Can_noticeperiod" ErrorMessage="The noptice period can't be less than 0 " ForeColor="Red" MaximumValue="360" MinimumValue="0"></asp:RangeValidator>
        </div>
         <div class="form-group">
        Attach CV *:
       <asp:FileUpload ID="FileUpload2" runat="server"/>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator10" runat="server" ControlToValidate="FileUpload2" ForeColor="Red" ErrorMessage="Please Attach CV"></asp:RequiredFieldValidator>
        </div>
      <div class="form-group">
          <%--<Button type="button"  class="btn btn-primary" ID="BTN_Submit"> Submit your request</Button>--%>
          <asp:Button class="btn btn-primary" ID="BTN_Submit" Text="Submit your request" runat="Server" OnClick="BTN_Submit_Click" />
        </div>

    </asp:Panel>
   </form> 
</div>




    



        
