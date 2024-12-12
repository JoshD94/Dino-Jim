(** [Skin] is an interface for skins used in the game. They have 1 function
    which draws the skin. *)
module type Skin = sig
  val draw : float -> float -> int
  (** [draw x y] draws the skin at ([x],[y]) *)
end

module DefaultSkin : Skin
(** [DefaultSkin] Contains the default skin draw function. *)

module SantaJim : Skin
(** [SantaJim] Contains the Santa Jim skin draw function. *)

module AngryJim : Skin
(** [AngryJim] Contains the Angry Jim skin draw function. *)

module GreenJim : Skin
(** [GreenJim] Contains the Green Jim skin draw function. *)

module RedJim : Skin
(** [RedJim] Contains the Red Jim skin draw function. *)

module BlueJim : Skin
(** [BlueJim] Contains the Blue Jim skin draw function. *)

module InvisibleJim : Skin
(** [InvisibleJim] Contains the Invisible Jim skin draw function. *)

module OrangeJim : Skin
(** [OrangeJim] Contains the Orange Jim skin draw function. *)

module DarthJim : Skin
(** [DarthJim] Contains the Darth Jim skin draw function. *)

module MagentaJim : Skin
(** [MagentaJim] Contains the Magenta Jim skin draw function. *)

module YellowJim : Skin
(** [YellowJim] Contains the Yellow Jim skin draw function. *)

module PurpleJim : Skin
(** [PurpleJim] Contains the Purple Jim skin draw function. *)
