with Athena.Cargo;

package Athena.Managers.Transportation is

   function Default_Transportation_Manager
     return Root_Manager_Type'Class;

   function Transport_Message
     (Empire   : Athena.Handles.Empire_Reference;
      From     : Athena.Handles.Star_Reference;
      To       : Athena.Handles.Star_Reference;
      Cargo    : Athena.Cargo.Cargo_Container;
      Priority : Athena.Handles.Order_Priority)
      return Message_Type'Class;

end Athena.Managers.Transportation;
