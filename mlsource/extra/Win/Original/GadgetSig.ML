(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)


signature GadgetSig = 
sig

   type action
   val actions : action list -> action
   type label
   type position

   datatype gadget =
      Gadget of ProcessID -> window -> position -> gadgetresult

   val forkedGadget : gadget -> gadget

   val runAppProc : (action -> gadget) -> position -> gadgetresult


   datatype 'a winAttrib = WinBackground of Color |
      WinClassStyle of ClassStyle list | WinCursor of CursorIdentifier |
      WinExStyle of WindowStyleExtended list | WinIcon of IconIdentifier |
      WinKey of KeyMessage * window * 'a -> 'a * action |
      WinMenuBar of 'a menuItem list |
      WinMessage of OtherMessage * window * 'a -> 'a * action |
      WinMouse of MouseMessage * window * 'a -> 'a * action |
      WinName of string |
      WinRefresh of RefreshMessage * window * 'a -> 'a * action |
      WinScroll of ScrollMessage * window * 'a -> 'a * action |
      WinSize of int * int | WinStyle of WindowStyle list

   val winGadgetBasic : '_a winAttrib list -> '_a -> child list -> gadget

   val winGadget :
      '_a winAttrib list ->
      '_a ->
      child list ->
      (MLHWND -> 'b * '_a -> '_a * action) -> 'b inport -> gadget

   datatype 'a buttonAttrib = ButtonFunction of window * 'a -> 'a * action |
      ButtonId of Id | ButtonLabel of string | ButtonSize of int * int |
      ButtonStyle of ButtonStyle list

   val buttonGadgetBasic : '_a buttonAttrib list -> '_a -> gadget

   val buttonGadget :
      '_a buttonAttrib list ->
      '_a -> (MLHWND -> 'b * '_a -> '_a * action) -> 'b inport -> gadget


   datatype 'a scrollBarAttrib = ScrollFunction of window * 'a -> 'a * action
      | ScrollId of Id | ScrollInitVal of int |
      ScrollOrientation of Orientation | ScrollRange of int * int |
      ScrollSize of int * int | ScrollStyle of ScrollBarStyle list

   val scrollGadgetBasic : '_a scrollBarAttrib list -> '_a -> gadget


   datatype 'a DlgAction = ActionCancel |
      ActionHelp of ((unit -> action) -> gadget) * position | ActionNo |
      ActionOk of 'a | ActionYes | PushButtonSize of int * int

   val dialogGadget :
      'a DlgAction list ->
      '_b winAttrib list ->
      '_b ->
      child list ->
      (MLHWND -> 'c * '_b -> '_b * action) ->
      'c inport -> ('a DlgResult -> action) -> gadget

   val localPort : ('_a port -> gadget) -> gadget




   val makeKeyCB :
      (KeyMessage * 'a * 'b -> 'b * action) ->
      'a -> 'b ref -> callbacktype * (System -> unit)


   val makeScrollCB :
      (ScrollMessage * 'a * 'b -> 'b * action) ->
      'a -> 'b ref -> callbacktype * (System -> unit)




   val runAppProc : (action -> gadget) -> position -> gadgetresult

   val startGadget :
      gadget -> ProcessID -> window -> position -> gadgetresult

   val mkCommand : (unit -> 'a) -> 'a command

   val nobehaveGadget :
      (('a -> 'b * 'c -> 'c * action) -> '_d inport -> 'e) -> 'e

   datatype Orientation = OHorizontal | OVertical

   val returnC : 'a -> 'a command

   val createMenu :
      'a menuItem list -> MLHMENU * (Id * (window * 'a -> 'a * action)) list

   val makeControlCB :
      Id * ('a * 'b -> 'b * action) ->
      'a -> 'b ref -> callbacktype * ('c -> unit)



   val runGadget : gadget -> position -> gadgetresult
   val mkAction : (unit -> unit) -> action

   val editGadget :
      '_a editAttrib list ->
      '_a -> (MLHWND -> 'b * '_a -> '_a * action) -> 'b inport -> gadget
 
   datatype 'a staticAttrib = StaticLabel of string | StaticSize of int * int
      | StaticStyle of StaticStyle list
   datatype 'a menuItem = MenuItem of label * 'a menuAttrib list |
      Popup of label * 'a menuItem list | Separator

   val comboboxGadgetBasic : '_a comboBoxAttrib list -> '_a -> gadget

   val noAction : action


   val runDialog : (('_a -> action) -> gadget) -> position -> '_a command

   val groupGadget : child list -> gadget

   datatype 'a menuAttrib = ItemBarBreak | ItemBreak | ItemChecked |
      ItemDisabled | ItemId of Id |
      MenuFunction of window * 'a -> 'a * action
   datatype 'a listBoxAttrib = ListBoxFunction of window * 'a -> 'a * action
      | ListBoxId of Id | ListBoxLabel of string | ListBoxSize of int * int |
      ListBoxStyle of ListBoxStyle list



   datatype gadgetresult =
      GR of
       {kill: action, show: action, close: action,
         children: gadgetresult list, callbacks: callbackcase list,
         winaction: (window -> action) -> action}

   datatype 'a comboBoxAttrib =
      ComboBoxFunction of window * 'a -> 'a * action | ComboBoxId of Id |
      ComboBoxLabel of string | ComboBoxSize of int * int |
      ComboBoxStyle of ComboBoxStyle list

   type 'a port

   datatype 'a editAttrib = EditFunction of window * 'a -> 'a * action |
      EditId of Id | EditLabel of string | EditSize of int * int |
      EditStyle of EditStyle list
   val send : 'a outport -> 'a -> action
   datatype 'a DlgResult = BoolResult of bool | NoResult | OkResult of 'a

   val makeRefreshCB :
      (RefreshMessage * 'a * 'b -> 'b * action) ->
      'a -> 'b ref -> callbacktype * (System -> unit)

   val listboxGadget :
      '_a listBoxAttrib list ->
      '_a -> (MLHWND -> 'b * '_a -> '_a * action) -> 'b inport -> gadget

   val seqC : 'a command * 'b command -> 'b command
   val thenC : 'a command * ('a -> 'b command) -> 'b command

   val editGadgetBasic : '_a editAttrib list -> '_a -> gadget

   datatype child = Abstract of gadget | Offset of position * gadget
   val listboxGadgetBasic : '_a listBoxAttrib list -> '_a -> gadget

   datatype 'a inport = In of 'a port
   val inport : 'a port -> 'a inport



   type 'a command
   val abstractGadget :
      '_a -> ('b * '_a -> '_a * action) -> 'b inport -> gadget
   val staticGadget : 'a staticAttrib list -> gadget
   type callbackcase
   val connectIn : 'a inport -> 'a handler -> unit
   datatype 'a outport = Out of 'a port
   val outport : 'a port -> 'a outport

   val loop : ('a inport -> 'a outport -> 'b) -> 'a port -> 'b

   val makeMouseCB :
      (MouseMessage * 'a * 'b -> 'b * action) ->
      'a -> 'b ref -> callbacktype * (System -> unit)

   val timerGadget : unit command -> timerControl inport -> gadget
  
   val scrollGadget :
      '_a scrollBarAttrib list ->
      '_a -> (MLHWND -> 'b * '_a -> '_a * action) -> 'b inport -> gadget

   val execute : 'a command -> 'a


   val newport : unit -> '_a port

   datatype timerControl = TimerKill | TimerRepeat of int



   val comboboxGadget :
      '_a comboBoxAttrib list ->
      '_a -> (MLHWND -> 'b * '_a -> '_a * action) -> 'b inport -> gadget
   end

end

