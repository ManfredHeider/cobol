      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22/06/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0477.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0477.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:16:04   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:50:42   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jun 22 2015 17:18:36   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-LEBENKAPENTNAHME.
           10 LKEVERTRAGSID                 PIC  X(0013).
           10 LKEKAPENTNNR                  PIC S9(4) COMP.
           10 LKEKAPENTNBETRAG              PIC S9(11)V9(2) COMP-3.
           10 LKEKAPENTNDAT                 PIC  X(010).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).


