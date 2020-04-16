
(*
                Component Behaviors of an ATM Machine

The functions here represent the component behaviors that an ATM
machine can take, including: prompting for and acquiring from the
customer some information (choosing an action or entering an account
id or an amount); presenting information to the customer; dispensing
cash.

Implementation of these behaviors is likely to require some database
of accounts, each with an id number, a customer name, and a current
balance.
 *)

(*....................................................................
 Initializing database of accounts
*)

(* initialize accts -- Establishes a database of accounts, each with a
   name, aribtrary id, and balance. The names and balances are
   initialized as per the `accts` provided. *)

let database = ref [] ;;

let initialize (acc : account_spec list) : unit =
  database := acc ;;

(*....................................................................
 Acquiring information from the customer
*)

(* acquire_id () -- Requests from the ATM customer and returns an id
   (akin to entering one's ATM card), by prompting for an id number
   and reading an id from stdin. *)
let acquire_id () =
  Printf.printf("Please enter an ID: \n");
  read_int ();;

(* acquire_amount () -- Requests from the ATM customer and returns an
   amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount : int =
  Printf.printf("Please enter an amount: \n");
  read_int () ;;

(* acquire_act () -- Requests from the user and returns an action to
   be performed, as a value of type action *)
val acquire_act : unit -> action ;;

(*....................................................................
  Querying and updating the account database

  These functions all raise Not_found if there is no account with the
  given id.
 *)

(* get_balance id -- Returns the balance for the customer account with
   the given id. *)
val get_balance : id -> int ;;

(* get_name id -- Returns the name associated with the customer
   account with the given id. *)
let get_name (i : id) : string =
  let rec help (i : id) (db : account_spec list) : string =
    match db with
    | [] -> raise (Invalid_argument "no account with this ID")
    | hd :: tl -> if hd.id = id then hd.name else help i tl in
    help i !database ;;

(* update_balance id amount -- Modifies the balance of the customer
   account with the given id, setting it to the given amount. *)
let update_balance (n : id) (i : int) : unit =
  get_balance n;;

(*....................................................................
  Presenting information and cash to the customer
 *)

(* present_message message -- Presents to the customer (on stdout) the
   given message followed by a newline. *)
val present_message : string -> unit ;;

(* deliver_cash amount -- Dispenses the given amount of cash to the
   customer (really just prints to stdout a message to that
   effect). *)
let deliver_cash (amt : int) : unit =
  Printf.printf "%i dollars dispensed" amt ;;
