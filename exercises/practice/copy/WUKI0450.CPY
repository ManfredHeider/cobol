      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0450.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUKI0450.CPv  $
      *
      *   Rev 2.3   Nov 26 2012 15:28:22   T13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.2   Oct 19 2012 13:40:10   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:28:04   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 16:41:34   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-CKVVERVORSCHLAG.
           10 VVSVERTRAGSID                 PIC  X(0013).
           10 VVSVERTRAGSNUMMER             PIC  X(0015).
           10 NIL-VVSVERTRAGSNUMMER         PIC  X(01).
           10 VVSVTRGERHOEHPRZ              PIC S9(7)V9(2) COMP-3.
           10 NIL-VVSVTRGERHOEHPRZ          PIC  X(01).
           10 VVSVTRGBEITRRUEK              PIC S9(7)V9(2) COMP-3.
           10 NIL-VVSVTRGBEITRRUEK          PIC  X(01).
           10 VVSKUNDBINDVTRG               PIC  X(0001).
           10 VVSSTORNOHAFTUNG              PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
