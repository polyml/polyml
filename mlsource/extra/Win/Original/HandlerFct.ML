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

(*****************************************************************
 *****************************************************************
 *
 *   M E S S A G E    S C H E D U L E R   
 *
 *****************************************************************
 *****************************************************************)

signature importCInterfaceSig =
sig
    type sym
    type vol
    type Ctype
    type 'a Conversion

    val get_sym      : string -> string -> sym
    val address      : vol -> vol
    val alloc        : int -> Ctype -> vol

    val voidStar : Ctype
    val Cstruct  : Ctype list -> Ctype
    val Cint     : Ctype
    val Clong    : Ctype
    
    val toCint       : int -> vol
    val toClong      : int -> vol

    val call0   : sym -> 
       unit -> 'a Conversion 
      -> unit -> 'a

    val call1   : sym -> 
      'a Conversion -> 'b Conversion 
      -> 'a -> 'b

    val call2   : sym -> 
      'a Conversion 
    * 'b Conversion -> 'c Conversion 
    -> 'a * 'b -> 'c
    
    val call3 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion -> 'd Conversion
    -> 'a * 'b * 'c -> 'd
    
    val call4 : sym ->
      'a Conversion
    * 'b Conversion
    * 'c Conversion
    * 'd Conversion -> 'e Conversion
    -> 'a * 'b * 'c * 'd -> 'e

    val call5 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion -> 'f Conversion
    -> 'a * 'b * 'c * 'd * 'e -> 'f

    val call6 : sym ->
      'a Conversion 
    * 'b Conversion 
    * 'c Conversion 
    * 'd Conversion 
    * 'e Conversion 
    * 'f Conversion -> 'g Conversion
    -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g

    val STRUCT4 : 'a1 Conversion * 'a2 Conversion * 
              'a3 Conversion * 'a4 Conversion
             -> ('a1 * 'a2 * 'a3 * 'a4) Conversion

    val INT     : int Conversion
    val LONG    : int Conversion
    val BOOL    : bool Conversion
    val VOID    : unit Conversion
    val POINTER : vol Conversion

end;

signature importTypeSig =
sig
  type vol
  type 'a Conversion
  datatype MLHWND = MLHWND of vol
  val HWND : MLHWND Conversion
  datatype MLHDC = MLHDC of vol
  val HDC : MLHDC Conversion

  datatype MLHMENU = MLHMENU of vol
end;

signature importMsgFnSig =
sig
  type System
  type MLHWND
  val MakeSystem : MLHWND*int*int*int -> System 
end;
 
signature importUserFnSig =
sig
   type MLHWND
   val DestroyWindow : MLHWND -> bool
end;

signature importDirSig =
sig
   val wpolyname   : string 
   val gdilibname  : string
   val userlibname : string
end;

functor HandlerFct ( structure Dir : importDirSig
                     structure CInterface : importCInterfaceSig 
                     structure Type : importTypeSig
                     structure MsgFn : importMsgFnSig
                     structure UserFn : importUserFnSig

                     sharing type CInterface.vol =
                                  Type.vol
                     sharing type CInterface.Conversion =
                                  Type.Conversion
                     sharing type Type.MLHWND =
                                  MsgFn.MLHWND =
                                  UserFn.MLHWND
                   ) = 
struct

open PolyML
open CInterface 
open Type
open MsgFn
open Process
(*open System*)
open UserFn
open Dir

  val get = get_sym userlibname
  val executable = wpolyname

  fun intop (f:int*int->'a) = f
  val op < = intop (op <)
  val op > = intop (op >)

  (* Message values in RTS queue *)
  val POLYMORPHIC = 1025
  val DELAYED = 1026
  val DESTROY_PROCESS_MESSAGE = 1030

  (* Masks used by GetQueueLength *)
  datatype mask = ALL         (* All messages *)
                | SYSALL      (* All System messages *)
                | SYS of int  (* Specific System messages *)
                | ALPHA       (* Polymorphic messages *)
                | EXPIRED     (* Delayed messages which have expired and are
                                 in R.T.S. Local Queue *)
                | PENDING     (* Delayed messages which have not expired yet
                                 and are kept in R.T.S. DList *)

                                                                            
  type ProcessID = int
  type WindowID = int
  type delay = int

  
  type window = MLHWND

  fun nullMenu () = MLHMENU (toCint 0)


  (* temporary to create null windows 
     pointer values are small integers 
     should not clash with normal pointer values *)
   
  local
   val WID = ref 0
  in
   fun nullWindow () = MLHWND (toCint (WID:=1 + (!WID) ; !WID))
  end


  val Qmessage = STRUCT4 (HWND,INT,INT,LONG)

  exception PROCESSERROR of { pid : int, fname : string, message : string } 
  exception WINDOWERROR  of { wid : int, fname : string, message : string } 

  val WID = ref 0; (* First WindowID *)
  val PID = ref 0; (* First ProcessID *)

  fun newProcessID () = (PID:=1 + (!PID) ; !PID)
  fun newWindowID ()  = (WID:=1 + (!WID) ; !WID)


  exception Windows of string


  (********* B A S I C  M E S S A G E  H A N D L I N G  *********)
  
  val IsEmptyLocalQueueRTS =
       call1 (get_sym executable "IsEmptyLocalQueue") (INT) (BOOL) 

  val GetLocalQueueRTS = 
      call1 (get_sym executable "GetLocalQueue") (INT) (Qmessage)

  val PostLocalQueueRTS = 
      call5 (get_sym executable "PostLocalQueue") (INT,HWND,INT,INT,LONG) (BOOL)

  val ProcessLocalDListRTS = 
      call1 (get_sym executable "ProcessLocalDLIST") (INT) (BOOL)

  val PutLocalDListRTS = 
      call6 (get_sym executable "PutLocalDLIST") (INT,HWND,INT,INT,LONG,LONG) (BOOL)

  val GetTime = 
      call0 (get "GetCurrentTime") () (LONG)

  val PeekMessage = 
      call5 (get "PeekMessageA") (POINTER,HWND,INT,INT,INT) (BOOL)

  val TranslateMessage =
      call1 (get "TranslateMessage") (POINTER) (BOOL)

  val DispatchMessage = 
      call1 (get "DispatchMessageA") (POINTER) (LONG)

  val GetwidRTS =
      call1 (get_sym executable "Getwid") (HWND) (INT)

  val GetpidRTS =
      call1 (get_sym executable "Getpid") (HWND) (INT)

  val GethwndRTS = 
      call1 (get_sym executable "GetHWND") (INT) (HWND)

  val AddProcessRTS =
      call1 (get_sym executable "AddProcess") (INT) (BOOL) 

  val RemoveProcessRTS =
      call1 (get_sym executable "RemoveProcess") (INT) (BOOL) 

  fun AddWindowRTS (w,p,drawable) =
  let
    val wid = newWindowID ()
  in
    call4 (get_sym executable "AddWindow") 
          (INT,HWND,INT,BOOL) (BOOL) 
          (wid,w,p,drawable) 
  end

  val RemoveWindowRTS = 
      call1 (get_sym executable "RemoveWindow") (INT) (BOOL)

  val SetProcessRunStateRTS =
      call2 (get_sym executable "SetProcessRunState") (INT,INT) (VOID)

  val EnableProcessWindowsRTS = 
      call2 (get_sym executable "EnableProcessWindows") (INT,BOOL) (VOID)

  val GetDeviceContextRTS =
      call1 (get_sym executable "GetDeviceContext") (HWND) (HDC) 

  val HasValidDeviceContextRTS =
      call1 (get_sym executable "HasValidDeviceContext") (HWND) (BOOL)

  (******* A D V A N C E D  M E S S A G E  H A N D L I N G  *******)

  val PriorityPostLocalQueueRTS = 
      call5 (get_sym executable "PriorityPostLocalQueue") 
            (INT,HWND,INT,INT,LONG) (BOOL)  

  val GetLengthLocalQueueRTS =
      call3 (get_sym executable "GetLengthLocalQueue") 
            (INT,HWND,INT) (INT) 

  val GetLengthLocalDListRTS = 
      call2 (get_sym executable "GetLengthLocalDList") 
            (INT,HWND) (INT) 
          
  val PeekLocalQueueRTS = 
      call1 (get_sym executable "PeekLocalQueue") (INT) (Qmessage) 

  val MessageExistsLocalQueueRTS =
       call3 (get_sym executable "MessageExistsLocalQueue") 
            (INT,HWND,INT) (BOOL) 

  val SelectMessageLocalQueueRTS =
       call3 (get_sym executable "SelectMessageLocalQueue") 
            (INT,HWND,INT) (Qmessage) 

  (*****************************************************************)

 
  abstype 'a Queue = Q of ('a list * 'a list)
  with
     exception EMPTY_QUEUE

     fun mkQ () = Q([],[])

     fun emptyQ ( Q([],[]) ) = true
     |   emptyQ _            = false

     fun putQ ( e , ( Q(inl,outl) ) ) = Q( e::inl , outl )

     fun putfrontQ ( e , ( Q(inl,outl) ) ) = Q( inl , e :: outl )

     fun getQ ( Q([],[]) ) = raise EMPTY_QUEUE
     |   getQ ( Q(inl,[]) ) = getQ ( Q([], rev inl) )
     |   getQ ( Q(inl,h::t) ) = (h, Q(inl,t) )
  end
  

  val (PROCESSTABLE : ( ( ProcessID
                          * (unit->unit) Queue            (* direct messages *)
                          * ( delay * (unit->unit) ) list (* delayed messages *)
                        ) list ref) ) = ref ([])

  
  val (CALLBACKTABLE : ( ( WindowID * (System->unit) ) list ref) ) = ref ([])


  (******************************)

  fun ProcessExistsML p =
  let
  fun pexists p ([]) = false
  |   pexists p ( (p1,q1,q2) :: rest ) = 
      if p=p1 then true
      else if p>p1 then false
      else pexists p rest
  in
    pexists p ( !PROCESSTABLE )
  end


  fun AddProcessEntryML p = 
  let
    fun ins p ([]) = [(p,mkQ(),[])]
    |  ins  p (l as ((p1,q1,q2) :: rest)) = 
    if p>p1 then (p,mkQ(),[]) :: l
    else if p<p1 then (p1,q1,q2) :: ins p rest
    else raise PROCESSERROR { pid = p,
                              fname = "AddProcessEntryML",
                              message = "Process already exists" } 
  in
     ( PROCESSTABLE := ins p (!PROCESSTABLE) )
  end


  fun RemoveProcessEntryML p = 
  let
     fun del p [] = []
     |   del p (l as (p1,q1,q2) :: rest) =
     if p > p1 then raise PROCESSERROR { pid = p,
                                         fname = "RemoveProcessEntryML",
                                         message = "Process does not exist" }
     else if p < p1 then (p1,q1,q2) :: del p rest
     else rest 
  in
     ( PROCESSTABLE := del p (!PROCESSTABLE) )
  end


  fun WindowExistsML w =
  let
  fun wexists w ([]) = false
  |   wexists w ( (w1,s) :: rest ) = 
      if w=w1 then true
      else if w>w1 then false
      else wexists w rest
  in
    wexists (GetwidRTS w) ( !CALLBACKTABLE )
  end


  fun AddCallbackML (w,send) =
  let
     fun ins (w,s) ([]) = [(w,send)]
     |   ins (w,s) (l as ((w1,s1) :: rest)) = 
     if w>w1 then [(w,s)] @ l
     else if w<w1 then (w1,s1) :: ins (w,s) rest
     else (w1,send) :: rest 
  in
     CALLBACKTABLE := ins ((GetwidRTS w),send) ( !CALLBACKTABLE ) 
  end


  fun DeleteCallbackML w =
  let
     fun del w [] = []
     |   del w (l as (w1,s1) :: rest) =
     if w > w1 then l
     else if w < w1 then (w1,s1) :: del w rest
     else rest 
  in
     CALLBACKTABLE := del (GetwidRTS w) ( ! CALLBACKTABLE ) 
  end 


  fun GetCallbackML wp = 
  let
     fun get w [] = raise WINDOWERROR {wid = GetwidRTS wp,
                                       fname = "GetCallbackML",
                                       message = "Window does not exist" }
     |   get w (l as (w1,s1) :: rest) =
     if w > w1 then raise WINDOWERROR {wid = GetwidRTS wp,
                                       fname = "GetCallbackML",
                                       message = "Window does not exist" }
     else if w < w1 then get w rest
     else s1 
  in
     get (GetwidRTS wp) ( ! CALLBACKTABLE ) 
  end 


  fun PostPolymorphicML p m = 
  let
    fun insert p m ([]) = raise PROCESSERROR { pid = p,
                                               fname = "PostPolymorphicML",  
                                               message = "Process does not exist" }
    |   insert p m ( (entry as (p1,q1,q2)) :: rest) = 
                          if p=p1 then ( (p1,putQ(m,q1),q2) :: rest)
                          else entry :: insert p m rest
  in
     PROCESSTABLE := insert p m ( !PROCESSTABLE )
  end


  fun PriorityPostPolymorphicML p m =
  let
    fun insert p m ([]) = raise PROCESSERROR { pid = p,
                                             fname = "PriorityPostPolymorphicML",  
                                             message = "Process does not exist" }
    |   insert p m ( (entry as (p1,q1,q2)) :: rest) = 
                          if p=p1 then ( (p1,putfrontQ(m,q1),q2) :: rest)
                          else entry :: insert p m rest
  in
     PROCESSTABLE := insert p m ( !PROCESSTABLE )
  end

  (*
  fun MovePolymorphicML (p,offset) = 
  let
    exception ERROR

    fun get n [] = raise ERROR
    |   get 1 (h::t) = (h,t)
    |   get n (h::t) = 
    let
      val (h2,rest) = get (n-1) t
    in 
      (h2,h::rest)
    end;
    
    fun swap n [] = raise ERROR
    |   swap 1 [h] = [h]
    |   swap n (h::t) =
    let
      val (out,rest) = get (n-1) t
    in
      out :: h :: rest
    end
     
    fun move p off ([]) = 
          raise PROCESSERROR { pid = p,
                               fname = "MovePolymorphicML",  
                               message = "Process does not exist" }
    |   move p off ( (entry as (p1,q1,q2)) :: rest) = 
                          if p=p1 then ( (p1, swap off q1,q2) :: rest)
                          else entry :: move p off rest
  in
     PROCESSTABLE := move p offset ( !PROCESSTABLE )
  end
  *)

  fun GetPolymorphicML p = 
  let
    fun del [] = raise PROCESSERROR { pid = p,
                                      fname = "GetPolymorphicML", 
                                      message = "Process queue is empty" }
    |   del (h::t) = (h,t) 

    fun delete p ([]) = raise PROCESSERROR { pid = p,
                                             fname = "GetPolymorphicML", 
                                             message = "Process does not exist" }
    |   delete p ((entry as (p1,q1,q2)) :: rest) = 
        if p=p1 then 
          let
             val (e,q) = getQ q1
          in 
             ( e , (p1,q,q2)::rest )
          end
        else 
          let
            val ( e , newtable ) = delete p rest
          in
             ( e , (p1,q1,q2) :: newtable )
          end
   
     val ( m , newtable ) = delete p ( !PROCESSTABLE )
  in
     ( PROCESSTABLE := newtable ; m  )
  end 


  fun PostDelayedML p delay m = 
  let
    val timeout = GetTime () + delay; 

    fun ins (t,m) ([]) = [(t,m)]
    |   ins (t,m) (l as ((t1,m1) :: rest)) = 
    if t<t1 then (t,m) :: l
    else (t1,m1) :: ins (t,m) rest

    fun insert p m ([]) = raise PROCESSERROR { pid = p,
                                               fname = "PostDelayedML",  
                                               message = "Process does not exist" }
    |   insert p m ( (entry as (p1,q1,q2)) :: rest) = 
                          if p=p1 then ( (p1,q1,ins (timeout,m) q2) :: rest)
                          else entry :: insert p m rest
  in
     PROCESSTABLE := insert p m ( !PROCESSTABLE )
  end


  fun MoveDelayedML (p,offset) = 
  let
    exception ERROR

    fun get n [] = raise ERROR
    |   get 1 (h::t) = (h,t)
    |   get n (h::t) = 
    let
      val (h2,rest) = get (n-1) t
    in 
      (h2,h::rest)
    end;
    
    fun swap n [] = raise ERROR
    |   swap 1 [h] = [h]
    |   swap n (h::t) =
    let
      val (out,rest) = get (n-1) t
    in
      out :: h :: rest
    end
     
    fun move p off ([]) = 
          raise PROCESSERROR { pid = p,
                               fname = "MoveDelayedML",  
                               message = "Process does not exist" }
    |   move p off ( (entry as (p1,q1,q2)) :: rest) = 
                          if p=p1 then ( (p1, q1,swap off q2) :: rest)
                          else entry :: move p off rest
  in
     PROCESSTABLE := move p offset ( !PROCESSTABLE )
  end

  fun GetDelayedML p = 
  let
    fun del [] = raise PROCESSERROR { pid = p,
                                      fname = "GetDelayedML", 
                                      message = "Process queue is empty" }
    |   del ((d,h)::t) = (h,t) 

    fun delete p ([]) = raise PROCESSERROR { pid = p,
                                             fname = "GetDelayedML", 
                                             message = "Process does not exist" }
    |   delete p ((entry as (p1,q1,q2)) :: rest) = 
        if p=p1 then 
          let
             val (h,t) = del q2
          in 
             ( h , (p1,q1,t)::rest )
          end
        else 
          let
            val ( h , newtable ) = delete p rest
          in
             ( h , (p1,q1,q2) :: newtable )
          end
   
     val ( m , newtable ) = delete p ( !PROCESSTABLE )
  in
     ( PROCESSTABLE := newtable ; m  )
  end 


  fun BuildQueues msg =
  if( PeekMessage(address msg,MLHWND (toCint 0),0,0,1)) then
       ( TranslateMessage (address msg) ;
         DispatchMessage(address msg) ; () )
   else ( waitMilliseconds(gettimeMilliseconds() + 1000);
          () )

  fun ProcessSystem (h,m,w,l) = 
              ( GetCallbackML h ) (MakeSystem (h,m,w,l))
   
  fun ProcessPolymorphic (h,m,w,l) = 
              ( GetPolymorphicML w ) ()

  fun ProcessDelayed (h,m,w,l) = 
              ( GetDelayedML w ) ()

  fun ProcessQueueML p = 
  if IsEmptyLocalQueueRTS p then true
  else
  let                           
       val (h,m,w,l) = GetLocalQueueRTS p
  in
      if (m <> DESTROY_PROCESS_MESSAGE) then 
       ( PolyML.exception_trace
          ( fn () => if m=POLYMORPHIC then ProcessPolymorphic (h,m,p,l)
                     else if m=DELAYED then ProcessDelayed (h,m,p,l)
                     else ProcessSystem (h,m,w,l) )
         handle Windows s => output (std_out,"Exception Windows <"^s^"> raised.\n")
       | _                => ();
         ProcessQueueML p 
       )
      else false
  end                           


  fun HANDLER p =
  let 
    val msg = alloc 1 
         (Cstruct [voidStar,Cint,Clong,Clong,Clong,Clong,Clong,Clong]);

    fun HANDLERLOCAL p =
      let val continue = ( ProcessLocalDListRTS p ;
                           ProcessQueueML p );
      in if continue then ( BuildQueues msg ;
                            HANDLERLOCAL p) 
                     else ()
      end
  in
    ( AddProcessRTS p;
      AddProcessEntryML p; 
      HANDLERLOCAL p;
      RemoveProcessRTS p;
      RemoveProcessEntryML p;
      ()  )
  end

  (***********************************************************
   *
   * A process terminates when it receives the message
   * DESTROYPROCESS in its local queue 
   * CLEANUP ???
   *
   * When a process dies the message queues associated
   * with the process are destroyed and also the windows
   * belonging to this process.
   *
   ***********************************************************)

  fun RunProcess p = ( fork (fn () => HANDLER p)  )
      
  fun delegate pid f
   = if ProcessExistsML pid
     then (PostPolymorphicML pid f;
           PostLocalQueueRTS (pid,MLHWND (toCint 0),POLYMORPHIC,pid,0))   (*****)
     else raise PROCESSERROR{ pid = pid, 
                              fname = "delegate",
                              message = "Attempt to send to inactive process"
                            }

  fun GetQueueLength p w m =
  if ProcessExistsML p
   then 
  ( case m of   
    (* All messages for window w in local queue *)
    ALL    => GetLengthLocalQueueRTS (p,GethwndRTS w,1030 )

    (* System messages for window w in local queue *)
  | SYSALL => GetLengthLocalQueueRTS (p,GethwndRTS w,1031 )

    (* Specific system messages for window w in local queue *)
  | SYS n => GetLengthLocalQueueRTS (p,GethwndRTS w,n)

    (* Polymorphicn messages for window w in local queue *)
  | ALPHA => GetLengthLocalQueueRTS (p,GethwndRTS w,POLYMORPHIC)

    (* Delayed messages for window w which have expired in local queue *)
  | EXPIRED => GetLengthLocalQueueRTS (p,GethwndRTS w,DELAYED)

     (* Delayed messages for window w which have not expired yet *)
  | PENDING => GetLengthLocalDListRTS (p,GethwndRTS w)  )
   else 
     raise PROCESSERROR{ pid = p,
                         fname = "GetQueueLength",
                         message = "Attemp to access inactive process"};
                              

  fun PeekNext p =
  if not (ProcessExistsML p) then
    raise PROCESSERROR{ pid = p,
                        fname = "PeekNext",
                        message = "Attemp to access inactive process"}
  else if (IsEmptyLocalQueueRTS p) then
    raise PROCESSERROR{ pid = p,
                        fname = "PeekNext",
                        message = "Attemp to Peek from empty queue"}
  else
    let 
      val (h,m,w,l) = PeekLocalQueueRTS p
      val w = GetwidRTS h
    in
       (w,m)
    end


  fun MessageExists p w m =
  if not (ProcessExistsML p) then
    raise PROCESSERROR{ pid = p,
                        fname = "MessageExists",
                        message = "Attemp to access inactive process"}
  else if not (WindowExistsML w) then 
    raise WINDOWERROR{ wid = (GetwidRTS w),
                       fname = "MessageExists",
                       message = "Attemp to access inesistent window"}
  else
    let
      val h = w
    in 

     ( case m of    
        (* All messages for window w in local queue *)
        ALL    => MessageExistsLocalQueueRTS (p,h,1030)

        (* System messages for window w in local queue *)
      | SYSALL => MessageExistsLocalQueueRTS (p,h,1031)

        (* Specific system messages for window w in local queue *)
      | SYS n => MessageExistsLocalQueueRTS (p,h,n)

        (* Polymorphicn messages for window w in local queue *)
      | ALPHA => MessageExistsLocalQueueRTS (p,h,POLYMORPHIC)

        (* Delayed messages for window w which have expired in local queue *)
      | EXPIRED => MessageExistsLocalQueueRTS (p,h,DELAYED)

        (* Delayed messages for window w which have not expired yet *)
      | PENDING => raise PROCESSERROR{ pid = p,
          fname = "MessageExists",
          message = "Pending messages are not placed in the queues yet!"} )
    end

  (*
  fun SelectMessage p w m =
  if not (ProcessExistsML p) then
    raise PROCESSERROR{ pid = p,
                        fname = "SelectMessage",
                        message = "Attemp to access inactive process"}
  else if not (WindowExistsML w) then 
    raise WINDOWERROR{ wid = GetwidRTS w,
                       fname = "SelectMessage",
                       message = "Attemp to access inexistent window"}
  else
     let
       val h = w
       
       val (h2,m,p1,p2) = 
         ( case m of    
            ALL     => SelectMessageLocalQueueRTS (p,h,1030 )
          | SYSALL  => SelectMessageLocalQueueRTS (p,h,1031 )
          | SYS n   => SelectMessageLocalQueueRTS (p,h,n)
          | ALPHA   => SelectMessageLocalQueueRTS (p,h,POLYMORPHIC)
          | EXPIRED => SelectMessageLocalQueueRTS (p,h,DELAYED)
          | PENDING => raise PROCESSERROR{ pid = p,
              fname = "SelectMessage",
              message = "Pending messages are not placed in the queues yet!"} 
         )
       val w2 = GetwidRTS h2
     in
       if(w2=0) then false
       else if m=POLYMORPHIC then (MovePolymorphicML (p,p1) ; true) 
       else if m=DELAYED then (MoveDelayedML (p,p1) ; true) 
       else true
     end
   *)

  fun profile () =
  let
   val printProcTable = 
     call0 (get_sym executable "printProcTable")
     () (VOID)

   val printWinTable = 
     call0 (get_sym executable "printWinTable")
     () (VOID)
  in
  ( printProcTable () ;
    printWinTable () ;
    ExtendedIO.flush_out std_out    ;
    (!PROCESSTABLE,!CALLBACKTABLE); () )
  end


  fun killprocess (pid : ProcessID) = 
                 ( PriorityPostLocalQueueRTS 
                   (pid,MLHWND (toCint 0),DESTROY_PROCESS_MESSAGE,0,0); 
                   SetProcessRunStateRTS (pid,1);
                   ())
                                                                      
  fun closeDownWin (w:window) = ( DestroyWindow w;
                                  RemoveWindowRTS (GetwidRTS w);
                                  DeleteCallbackML w )

end;

