with Ada.Containers.Doubly_Linked_Lists;

package Athena.Ships.Lists is
  new Ada.Containers.Doubly_Linked_Lists
    (Athena.Handles.Ship.Ship_Handle,
     Athena.Handles.Ship."=");
