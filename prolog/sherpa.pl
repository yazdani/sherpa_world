
:- module(sherpa,
  [
      comp_networkQuality/2,
      comp_networkQuality_at_time/3
  ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl_parser')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('knowrob_owl')).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2, 'http://knowrob.org/kb/srdl2.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2comp, 'http://knowrob.org/kb/srdl2-comp.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2cap, 'http://knowrob.org/kb/srdl2-cap.owl#', [keep(true)]).
:- rdf_db:rdf_register_ns(srdl2act, 'http://knowrob.org/kb/srdl2-action.owl#', [keep(true)]).

:- rdf_meta
        comp_networkQuality(r,r),
        comp_networkQuality_at_time(r,r,r).

comp_networkQuality(Robot, Quality) :-
    get_timepoint(Instant),
    comp_networkQuality_at_time(Robot, Quality, Instant).

comp_networkQuality_at_time(_, Quality, _) :-
    Quality='http://knowrob.org/kb/knowrob.owl#NetworkQualityBad'.

%knowrob_temporal:holds(Robot, 'http://knowrob.org/kb/knowrob.owl#networkQuality', Quality, Interval) :-
%    spatially_holds_interval(Top, comp_above_of_at_time, Quality, Interval).
