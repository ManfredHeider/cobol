      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DBAN0037.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DBAN0037.CPv  $
      *
      *   Rev 2.5   Feb 13 2007 19:30:50   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   Feb 13 2007 18:30:26   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 17:20:02   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   03 Jan 2007 18:54:06   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   22 Dec 2006 19:56:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   15 Dec 2006 15:58:54   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   MICRO FOCUS MAINFRAME EXPRESS  3.0.75
      *   MICRO FOCUS BMS SCREEN PAINTER (VER COBC_1-1-37-31)
      *   MAPSET NAME   BAN0037
      *   DATE CREATED  02/07/2007
      *   TIME CREATED  14:26:53

      *  INPUT DATA FOR MAP BAN0037
         01 BAN0037I.
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
            03 EVBPRZL                        PIC S9(4) COMP.
            03 EVBPRZF                        PIC X.
            03 FILLER REDEFINES EVBPRZF.
               05 EVBPRZA                        PIC X.
            03 FILLER                         PIC X(1).
            03 EVBPRZI                        PIC X(1).
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

      *  OUTPUT DATA FOR MAP BAN0037
         01 BAN0037O REDEFINES BAN0037I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TGESH                          PIC X.
            03 TGESO                          PIC X(28).
            03 FILLER                         PIC X(3).
            03 EGESNRH                        PIC X.
            03 EGESNRO                        PIC X(2).
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
            03 EVBPRZH                        PIC X.
            03 EVBPRZO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 TP34CH                         PIC X.
            03 TP34CO                         PIC X(28).
            03 FILLER                         PIC X(3).
            03 EP34CH                         PIC X.
            03 EP34CO                         PIC X(7).
            03 FILLER                         PIC X(3).
            03 AFCZANZH                       PIC X.
            03 AFCZANZO                       PIC X(79).

