module type S = sig
  type t
  (** abstract; should represent some sort of discrete physical object
        it's possible to embroider with. *)

  val pp : Format.formatter -> t -> unit
  val parse : string -> (t, [`Msg of string]) result

  val to_rgb : t -> (int * int * int)
  (** representation of the thread in RGB color space for screen display.*)

  val of_rgb : (int * int * int) -> t option
  (** attempts an "exact" reverse lookup from RGB to a single thread.
      If this is not possible (because no thread is available for that color,
      or because multiple threads map there), return None. *)

  val to_string : t -> string
  (** human-readable string describing the thread. *)

  val of_string : string -> t option
  (** attempts to find a thread matching the description.  Pass the thread color
      or description with no "DMC" prefix (e.g., "310", not "DMC 310". *)

  val to_yojson : t -> Yojson.Safe.t
  (** generate a json representation of this thread. *)

  val of_yojson : Yojson.Safe.t -> (t, string) result
  (** get a t from some json, maybe. *)

  val equal : t -> t -> bool
  (** hold up two threads to the light.  look carefully. *)

  val neighbors : t -> t list
  (** show colors in the same color family as this thread. *)

  val basic : t list
  (** the most readily available threads from this vendor. If there's
      a cheap starter set of threads, these are the threads in it.
      Various tools use `List.hd basic` as a default thread color,
      so `Not_found` exceptions may be caused by leaving this empty.
  *)

  val all : t list
  (** all the threads for this vendor. *)

  (* TODO: it would very likely be a good idea to have some kind of
     of_rgb_approximate available, likewise one can imagine of_rgb ~common_only:true, etc. *)
end

module type Floss_Vendor = sig
  module Thread : S
  val name : string
(** The name of the vendor, in the format most widely recognized by vendors.
    Currently English-only (oh no i18n!). *)

  (* TODO: one can imagine purchase link constructors, etc here. *)
end
