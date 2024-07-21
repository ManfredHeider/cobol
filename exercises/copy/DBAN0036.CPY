      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0036.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DBAN0036.CPv  $
      *
      *   Rev 2.4   Feb 13 2007 19:30:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 18:30:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 13 2007 17:19:48   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   22 Dec 2006 19:53:40   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   15 Dec 2006 15:48:40   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-31)
      *   MAPSET NAME   BAN0036
      *   DATE CREATED  01/11/2007
      *   TIME CREATED  09:19:18

      *  INPUT DATA FOR MAP BAN0036
         01 BAN0036I.
            03 FILLER                         PIC X(12).
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
            03 TPINL                          PIC S9(4) COMP.
            03 TPINF                          PIC X.
            03 FILLER REDEFINES TPINF.
               05 TPINA                          PIC X.
            03 FILLER                         PIC X(1).
            03 TPINI                          PIC X(28).
            03 EVNRL                          PIC S9(4) COMP.
            03 EVNRF                          PIC X.
            03 FILLER REDEFINES EVNRF.
               05 EVNRA                          PIC X.
            03 FILLER                         PIC X(1).
            03 EVNRI                          PIC X(15).
            03 TVORSCL                        PIC S9(4) COMP.
            03 TVORSCF                        PIC X.
            03 FILLER REDEFINES TVORSCF.
               05 TVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TVORSCI                        PIC X(28).
            03 EVORSCL                        PIC S9(4) COMP.
            03 EVORSCF                        PIC X.
            03 FILLER REDEFINES EVORSCF.
               05 EVORSCA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVORSCI                        PIC X(17).
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
            03 TMCKUNL                        PIC S9(4) COMP.
            03 TMCKUNF                        PIC X.
            03 FILLER REDEFINES TMCKUNF.
               05 TMCKUNA                        PIC X.
            03 FILLER                         PIC X(1).
            03 TMCKUNI                        PIC X(28).
            03 EKDMCL                         PIC S9(4) COMP.
            03 EKDMCF                         PIC X.
            03 FILLER REDEFINES EKDMCF.
               05 EKDMCA                         PIC X.
            03 FILLER                         PIC X(1).
            03 EKDMCI                         PIC X(8).
            03 AFCZANZL                       PIC S9(4) COMP.
            03 AFCZANZF                       PIC X.
            03 FILLER REDEFINES AFCZANZF.
               05 AFCZANZA                       PIC X.
            03 FILLER                         PIC X(1).
            03 AFCZANZI                       PIC X(79).

      *  OUTPUT DATA FOR MAP BAN0036
         01 BAN0036O REDEFINES BAN0036I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TGESH                          PIC X.
            03 TGESO                          PIC X(28).
            03 FILLER                         PIC X(3).
            03 EGESNRH                        PIC X.
            03 EGESNRO                        PIC X(2).
            03 FILLER                         PIC X(3).
            03 TPINH                          PIC X.
            03 TPINO                          PIC X(28).
            03 FILLER                         PIC X(3).
            03 EVNRH                          PIC X.
            03 EVNRO                          PIC X(15).
            03 FILLER                         PIC X(3).
            03 TVORSCH                        PIC X.
            03 TVORSCO                        PIC X(28).
            03 FILLER                         PIC X(3).
            03 EVORSCH                        PIC X.
            03 EVORSCO                        PIC X(17).
            03 FILLER                         PIC X(3).
            03 TVBNRH                         PIC X.
            03 TVBNRO                         PIC X(28).
            03 FILLER                         PIC X(3).
            03 EVBNRH                         PIC X.
            03 EVBNRO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 TMCKUNH                        PIC X.
            03 TMCKUNO                        PIC X(28).
            03 FILLER                         PIC X(3).
            03 EKDMCH                         PIC X.
            03 EKDMCO                         PIC X(8).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).


