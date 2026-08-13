(* FIFO queue of orders at a price *)

open Order

type t = {
    price : float;
    mutable orders : order Queue.t;
}

let create price = { price; orders = Queue.create () }

let add_order level order = Queue.push order level.orders

let total_quantity level = Queue.fold (fun acc order -> acc + order.quantity) 0 level.orders

let is_empty level = Queue.is_empty level.orders

(* Removes an order by id in the queue, perserving FIFO order *)

let cancel_order level id = 
  let new_queue = Queue.create () in 
  let found = ref false in
  Queue.iter
    (fun order ->
      if order.id = id then found := true
      else Queue.push order new_queue)
      level.orders;
      level.orders <- new_queue;
      !found

let peek_front level = 
  if Queue.is_empty level.orders then None
  else Some (Queue.peek level.orders)

let pop_front level = 
  if Queue.is_empty level.orders then None
  else Some (Queue.pop level.orders)

