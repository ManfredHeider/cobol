      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0049.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DBAN0049.CPv  $
      *
      *   Rev 2.10   Aug 03 2016 14:38:24   t13ae23
      *AAN01278: Anpassungen/Änderungen Frankfurter Dialog DV
      *
      *   Rev 2.9   Feb 25 2016 10:07:48   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.8   Feb 22 2016 14:19:48   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.7   Aug 12 2015 14:48:56   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.6   Jun 22 2015 15:44:42   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.5   Jun 10 2015 17:38:48   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.4   Jun 10 2015 14:49:42   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 10 2015 14:47:48   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 14:46:12   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:06:40   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   27 Dec 2006 18:42:54   t13ae34
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *   MAINFRAME EXPRESS 3.1  6.013.0041
      *   MICRO FOCUS BMS SCREEN PAINTER (VER IDE_6-0-57)
      *   MAPSET NAME   BAN0049
      *   DATE CREATED  07/26/2016
      *   TIME CREATED  14:35:49

      *  INPUT DATA FOR MAP BAN0049
         01 BAN0049I.
            03 FILLER                         PIC X(12).
            03 TVBNRL                         PIC S9(4) COMP.
            03 TVBNRF                         PIC X.
            03 FILLER REDEFINES TVBNRF.
               05 TVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVBNRI                         PIC X(2).
            03 AVBNRL                         PIC S9(4) COMP.
            03 AVBNRF                         PIC X.
            03 FILLER REDEFINES AVBNRF.
               05 AVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNRI                         PIC X(7).
            03 AVBPRZL                        PIC S9(4) COMP.
            03 AVBPRZF                        PIC X.
            03 FILLER REDEFINES AVBPRZF.
               05 AVBPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVBPRZI                        PIC X(1).
            03 ADIRL                          PIC S9(4) COMP.
            03 ADIRF                          PIC X.
            03 FILLER REDEFINES ADIRF.
               05 ADIRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 ADIRI                          PIC X(3).
            03 AVBNAMEL                       PIC S9(4) COMP.
            03 AVBNAMEF                       PIC X.
            03 FILLER REDEFINES AVBNAMEF.
               05 AVBNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVBNAMEI                       PIC X(20).
            03 TVMNRL                         PIC S9(4) COMP.
            03 TVMNRF                         PIC X.
            03 FILLER REDEFINES TVMNRF.
               05 TVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVMNRI                         PIC X(2).
            03 AVMNRL                         PIC S9(4) COMP.
            03 AVMNRF                         PIC X.
            03 FILLER REDEFINES AVMNRF.
               05 AVMNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNRI                         PIC X(7).
            03 AVMPRZL                        PIC S9(4) COMP.
            03 AVMPRZF                        PIC X.
            03 FILLER REDEFINES AVMPRZF.
               05 AVMPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVMPRZI                        PIC X(1).
            03 AVMNAMEL                       PIC S9(4) COMP.
            03 AVMNAMEF                       PIC X.
            03 FILLER REDEFINES AVMNAMEF.
               05 AVMNAMEA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AVMNAMEI                       PIC X(20).
            03 TVORSCL                        PIC S9(4) COMP.
            03 TVORSCF                        PIC X.
            03 FILLER REDEFINES TVORSCF.
               05 TVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVORSCI                        PIC X(7).
            03 AVORSCL                        PIC S9(4) COMP.
            03 AVORSCF                        PIC X.
            03 FILLER REDEFINES AVORSCF.
               05 AVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 AVORSCI                        PIC X(17).
            03 TVNRL                          PIC S9(4) COMP.
            03 TVNRF                          PIC X.
            03 FILLER REDEFINES TVNRF.
               05 TVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TVNRI                          PIC X(11).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER REDEFINES EVNRF.
               05 EVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
            03 EVNRPZL                        PIC S9(4) COMP.
            03 EVNRPZF                        PIC X.
            03 FILLER REDEFINES EVNRPZF.
               05 EVNRPZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRPZI                        PIC X(2).
            03 TR-FORML                       PIC S9(4) COMP.
            03 TR-FORMF                       PIC X.
            03 FILLER REDEFINES TR-FORMF.
               05 TR-FORMA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TR-FORMI                       PIC X(17).
            03 ER-FORML                       PIC S9(4) COMP.
            03 ER-FORMF                       PIC X.
            03 FILLER REDEFINES ER-FORMF.
               05 ER-FORMA                       PIC X.
            03 FILLER                         PIC X(1).
            03 ER-FORMI                       PIC X(3).
            03 TFNLANGL                       PIC S9(4) COMP.
            03 TFNLANGF                       PIC X.
            03 FILLER REDEFINES TFNLANGF.
               05 TFNLANGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TFNLANGI                       PIC X(7).
            03 EFNLANGL                       PIC S9(4) COMP.
            03 EFNLANGF                       PIC X.
            03 FILLER REDEFINES EFNLANGF.
               05 EFNLANGA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EFNLANGI                       PIC X(70).
            03 TSTRL                          PIC S9(4) COMP.
            03 TSTRF                          PIC X.
            03 FILLER REDEFINES TSTRF.
               05 TSTRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TSTRI                          PIC X(9).
            03 ESTRL                          PIC S9(4) COMP.
            03 ESTRF                          PIC X.
            03 FILLER REDEFINES ESTRF.
               05 ESTRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 ESTRI                          PIC X(45).
            03 TPLZL                          PIC S9(4) COMP.
            03 TPLZF                          PIC X.
            03 FILLER REDEFINES TPLZF.
               05 TPLZA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPLZI                          PIC X(6).
            03 EFAPLZL                        PIC S9(4) COMP.
            03 EFAPLZF                        PIC X.
            03 FILLER REDEFINES EFAPLZF.
               05 EFAPLZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EFAPLZI                        PIC X(5).
            03 TORTL                          PIC S9(4) COMP.
            03 TORTF                          PIC X.
            03 FILLER REDEFINES TORTF.
               05 TORTA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TORTI                          PIC X(6).
            03 EFAORTL                        PIC S9(4) COMP.
            03 EFAORTF                        PIC X.
            03 FILLER REDEFINES EFAORTF.
               05 EFAORTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EFAORTI                        PIC X(50).
            03 TPOSTFL                        PIC S9(4) COMP.
            03 TPOSTFF                        PIC X.
            03 FILLER REDEFINES TPOSTFF.
               05 TPOSTFA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TPOSTFI                        PIC X(11).
            03 EPOSTFNL                       PIC S9(4) COMP.
            03 EPOSTFNF                       PIC X.
            03 FILLER REDEFINES EPOSTFNF.
               05 EPOSTFNA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPOSTFNI                       PIC X(10).
            03 TPOSTFPL                       PIC S9(4) COMP.
            03 TPOSTFPF                       PIC X.
            03 FILLER REDEFINES TPOSTFPF.
               05 TPOSTFPA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TPOSTFPI                       PIC X(12).
            03 EPOSTFPL                       PIC S9(4) COMP.
            03 EPOSTFPF                       PIC X.
            03 FILLER REDEFINES EPOSTFPF.
               05 EPOSTFPA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPOSTFPI                       PIC X(5).
            03 TPOSTFOL                       PIC S9(4) COMP.
            03 TPOSTFOF                       PIC X.
            03 FILLER REDEFINES TPOSTFOF.
               05 TPOSTFOA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TPOSTFOI                       PIC X(12).
            03 EPOSTFOL                       PIC S9(4) COMP.
            03 EPOSTFOF                       PIC X.
            03 FILLER REDEFINES EPOSTFOF.
               05 EPOSTFOA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EPOSTFOI                       PIC X(40).
            03 TTELEL                         PIC S9(4) COMP.
            03 TTELEF                         PIC X.
            03 FILLER REDEFINES TTELEF.
               05 TTELEA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TTELEI                         PIC X(8).
            03 ETELEL                         PIC S9(4) COMP.
            03 ETELEF                         PIC X.
            03 FILLER REDEFINES ETELEF.
               05 ETELEA                         PIC X.
            03 FILLER                         PIC X(1).
            03 ETELEI                         PIC X(30).
            03 TFAXL                          PIC S9(4) COMP.
            03 TFAXF                          PIC X.
            03 FILLER REDEFINES TFAXF.
               05 TFAXA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TFAXI                          PIC X(4).
            03 EFAXL                          PIC S9(4) COMP.
            03 EFAXF                          PIC X.
            03 FILLER REDEFINES EFAXF.
               05 EFAXA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EFAXI                          PIC X(30).
            03 TMOBILL                        PIC S9(4) COMP.
            03 TMOBILF                        PIC X.
            03 FILLER REDEFINES TMOBILF.
               05 TMOBILA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TMOBILI                        PIC X(6).
            03 EMOBILL                        PIC S9(4) COMP.
            03 EMOBILF                        PIC X.
            03 FILLER REDEFINES EMOBILF.
               05 EMOBILA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EMOBILI                        PIC X(30).
            03 T-EMAILL                       PIC S9(4) COMP.
            03 T-EMAILF                       PIC X.
            03 FILLER REDEFINES T-EMAILF.
               05 T-EMAILA                       PIC X.
            03 FILLER                         PIC X(1).
            03 T-EMAILI                       PIC X(7).
            03 EEMAILL                        PIC S9(4) COMP.
            03 EEMAILF                        PIC X.
            03 FILLER REDEFINES EEMAILF.
               05 EEMAILA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EEMAILI                        PIC X(50).
            03 TAMTGL                         PIC S9(4) COMP.
            03 TAMTGF                         PIC X.
            03 FILLER REDEFINES TAMTGF.
               05 TAMTGA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TAMTGI                         PIC X(12).
            03 EAMTGL                         PIC S9(4) COMP.
            03 EAMTGF                         PIC X.
            03 FILLER REDEFINES EAMTGF.
               05 EAMTGA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EAMTGI                         PIC X(50).
            03 THREGNRL                       PIC S9(4) COMP.
            03 THREGNRF                       PIC X.
            03 FILLER REDEFINES THREGNRF.
               05 THREGNRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 THREGNRI                       PIC X(19).
            03 EHREGNRL                       PIC S9(4) COMP.
            03 EHREGNRF                       PIC X.
            03 FILLER REDEFINES EHREGNRF.
               05 EHREGNRA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EHREGNRI                       PIC X(30).
            03 TUST-IDL                       PIC S9(4) COMP.
            03 TUST-IDF                       PIC X.
            03 FILLER REDEFINES TUST-IDF.
               05 TUST-IDA                       PIC X.
            03 FILLER                         PIC X(1).
            03 TUST-IDI                       PIC X(11).
            03 EUST-IDL                       PIC S9(4) COMP.
            03 EUST-IDF                       PIC X.
            03 FILLER REDEFINES EUST-IDF.
               05 EUST-IDA                       PIC X.
            03 FILLER                         PIC X(1).
            03 EUST-IDI                       PIC X(15).
            03 TKZVSTL                        PIC S9(4) COMP.
            03 TKZVSTF                        PIC X.
            03 FILLER REDEFINES TKZVSTF.
               05 TKZVSTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TKZVSTI                        PIC X(26).
            03 EKZVSTL                        PIC S9(4) COMP.
            03 EKZVSTF                        PIC X.
            03 FILLER REDEFINES EKZVSTF.
               05 EKZVSTA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EKZVSTI                        PIC X(1).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0049
         01 BAN0049O REDEFINES BAN0049I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TVBNRH                         PIC X.
            03 TVBNRO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AVBNRH                         PIC X.
            03 AVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVBPRZH                        PIC X.
            03 AVBPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 ADIRH                          PIC X.
            03 ADIRO                          PIC X(3).
            03 FILLER                         PIC X(3).
            03 AVBNAMEH                       PIC X.
            03 AVBNAMEO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVMNRH                         PIC X.
            03 TVMNRO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 AVMNRH                         PIC X.
            03 AVMNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVMPRZH                        PIC X.
            03 AVMPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AVMNAMEH                       PIC X.
            03 AVMNAMEO                       PIC X(20).
            03 FILLER                         PIC X(3).
            03 TVORSCH                        PIC X.
            03 TVORSCO                        PIC X(7).
            03 FILLER                         PIC X(3).
            03 AVORSCH                        PIC X.
            03 AVORSCO                        PIC X(17).
            03 FILLER                         PIC X(3).
            03 TVNRH                          PIC X.
            03 TVNRO                          PIC X(11).
            03 FILLER                         PIC X(3).
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
            03 FILLER                         PIC X(3).
            03 EVNRPZH                        PIC X.
            03 EVNRPZO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TR-FORMH                       PIC X.
            03 TR-FORMO                       PIC X(17).
            03 FILLER                         PIC X(3).
            03 ER-FORMH                       PIC X.
            03 ER-FORMO                       PIC X(3).
            03 FILLER                         PIC X(3).
            03 TFNLANGH                       PIC X.
            03 TFNLANGO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 EFNLANGH                       PIC X.
            03 EFNLANGO                       PIC X(70).
            03 FILLER                         PIC X(3).
            03 TSTRH                          PIC X.
            03 TSTRO                          PIC X(9).
            03 FILLER                         PIC X(3).
            03 ESTRH                          PIC X.
            03 ESTRO                          PIC X(45).
            03 FILLER                         PIC X(3).
            03 TPLZH                          PIC X.
            03 TPLZO                          PIC X(6).
            03 FILLER                         PIC X(3).
            03 EFAPLZH                        PIC X.
            03 EFAPLZO                        PIC X(5).
            03 FILLER                         PIC X(3).
            03 TORTH                          PIC X.
            03 TORTO                          PIC X(6).
            03 FILLER                         PIC X(3).
            03 EFAORTH                        PIC X.
            03 EFAORTO                        PIC X(50).
            03 FILLER                         PIC X(3).
            03 TPOSTFH                        PIC X.
            03 TPOSTFO                        PIC X(11).
            03 FILLER                         PIC X(3).
            03 EPOSTFNH                       PIC X.
            03 EPOSTFNO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 TPOSTFPH                       PIC X.
            03 TPOSTFPO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 EPOSTFPH                       PIC X.
            03 EPOSTFPO                       PIC X(5).
            03 FILLER                         PIC X(3).
            03 TPOSTFOH                       PIC X.
            03 TPOSTFOO                       PIC X(12).
            03 FILLER                         PIC X(3).
            03 EPOSTFOH                       PIC X.
            03 EPOSTFOO                       PIC X(40).
            03 FILLER                         PIC X(3).
            03 TTELEH                         PIC X.
            03 TTELEO                         PIC X(8).
            03 FILLER                         PIC X(3).
            03 ETELEH                         PIC X.
            03 ETELEO                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 TFAXH                          PIC X.
            03 TFAXO                          PIC X(4).
            03 FILLER                         PIC X(3).
            03 EFAXH                          PIC X.
            03 EFAXO                          PIC X(30).
            03 FILLER                         PIC X(3).
            03 TMOBILH                        PIC X.
            03 TMOBILO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 EMOBILH                        PIC X.
            03 EMOBILO                        PIC X(30).
            03 FILLER                         PIC X(3).
            03 T-EMAILH                       PIC X.
            03 T-EMAILO                       PIC X(7).
            03 FILLER                         PIC X(3).
            03 EEMAILH                        PIC X.
            03 EEMAILO                        PIC X(50).
            03 FILLER                         PIC X(3).
            03 TAMTGH                         PIC X.
            03 TAMTGO                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 EAMTGH                         PIC X.
            03 EAMTGO                         PIC X(50).
            03 FILLER                         PIC X(3).
            03 THREGNRH                       PIC X.
            03 THREGNRO                       PIC X(19).
            03 FILLER                         PIC X(3).
            03 EHREGNRH                       PIC X.
            03 EHREGNRO                       PIC X(30).
            03 FILLER                         PIC X(3).
            03 TUST-IDH                       PIC X.
            03 TUST-IDO                       PIC X(11).
            03 FILLER                         PIC X(3).
            03 EUST-IDH                       PIC X.
            03 EUST-IDO                       PIC X(15).
            03 FILLER                         PIC X(3).
            03 TKZVSTH                        PIC X.
            03 TKZVSTO                        PIC X(26).
            03 FILLER                         PIC X(3).
            03 EKZVSTH                        PIC X.
            03 EKZVSTO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


