      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0032.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DBAN0032.CPv  $
      *
      *   Rev 2.5   Feb 13 2007 19:28:48   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   Feb 13 2007 18:29:16   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 17:14:36   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   22 Dec 2006 19:48:04   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 06 2006 10:51:50   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Aug 16 2006 14:11:02   t13ae34
      *Archiverstellung
      *
      *   MICRO FOCUS BMS SCREEN PAINTER (VER MFER25)
      *   MAPSET NAME   BAN0032
      *   DATE CREATED  08/17/2006
      *   TIME CREATED  10:47:26

      *  INPUT DATA FOR MAP BAN0032
         01 BAN0032I.
            03 FILLER                         PIC X(12).
            03 TVBNRL                         PIC S9(4) COMP.
            03 TVBNRF                         PIC X.
            03 FILLER REDEFINES TVBNRF.
               05 TVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TVBNRI                         PIC X(28).
            03 EVBNRL                         PIC S9(4) COMP.
            03 EVBNRF                         PIC X.
            03 FILLER REDEFINES EVBNRF.
               05 EVBNRA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EVBNRI                         PIC X(7).
            03 EVBPRZL                        PIC S9(4) COMP.
            03 EVBPRZF                        PIC X.
            03 FILLER REDEFINES EVBPRZF.
               05 EVBPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVBPRZI                        PIC X(1).
            03 TGESL                          PIC S9(4) COMP.
            03 TGESF                          PIC X.
            03 FILLER REDEFINES TGESF.
               05 TGESA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TGESI                          PIC X(28).
            03 EGESNRL                        PIC S9(4) COMP.
            03 EGESNRF                        PIC X.
            03 FILLER REDEFINES EGESNRF.
               05 EGESNRA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EGESNRI                        PIC X(2).
            03 TPRTAL                         PIC S9(4) COMP.
            03 TPRTAF                         PIC X.
            03 FILLER REDEFINES TPRTAF.
               05 TPRTAA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TPRTAI                         PIC X(28).
            03 EPRTAL                         PIC S9(4) COMP.
            03 EPRTAF                         PIC X.
            03 FILLER REDEFINES EPRTAF.
               05 EPRTAA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EPRTAI                         PIC X(15).
            03 TEZWL                          PIC S9(4) COMP.
            03 TEZWF                          PIC X.
            03 FILLER REDEFINES TEZWF.
               05 TEZWA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TEZWI                          PIC X(28).
            03 EEZWL                          PIC S9(4) COMP.
            03 EEZWF                          PIC X.
            03 FILLER REDEFINES EEZWF.
               05 EEZWA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EEZWI                          PIC X(1).
            03 TP34CL                         PIC S9(4) COMP.
            03 TP34CF                         PIC X.
            03 FILLER REDEFINES TP34CF.
               05 TP34CA                         PIC X.
            03 FILLER                         PIC X(1).
            03 TP34CI                         PIC X(28).
            03 EP34CL                         PIC S9(4) COMP.
            03 EP34CF                         PIC X.
            03 FILLER REDEFINES EP34CF.
               05 EP34CA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EP34CI                         PIC X(7).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0032
         01 BAN0032O REDEFINES BAN0032I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TVBNRH                         PIC X.
            03 TVBNRO                         PIC X(28).
            03 FILLER                         PIC X(3).
            03 EVBNRH                         PIC X.
            03 EVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 EVBPRZH                        PIC X.
            03 EVBPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TGESH                          PIC X.
            03 TGESO                          PIC X(28).
            03 FILLER                         PIC X(3).
            03 EGESNRH                        PIC X.
            03 EGESNRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TPRTAH                         PIC X.
            03 TPRTAO                         PIC X(28).
            03 FILLER                         PIC X(3).
            03 EPRTAH                         PIC X.
            03 EPRTAO                         PIC X(15).
            03 FILLER                         PIC X(3).
            03 TEZWH                          PIC X.
            03 TEZWO                          PIC X(28).
            03 FILLER                         PIC X(3).
            03 EEZWH                          PIC X.
            03 EEZWO                          PIC X(1).
            03 FILLER                         PIC X(3).
            03 TP34CH                         PIC X.
            03 TP34CO                         PIC X(28).
            03 FILLER                         PIC X(3).
            03 EP34CH                         PIC X.
            03 EP34CO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

