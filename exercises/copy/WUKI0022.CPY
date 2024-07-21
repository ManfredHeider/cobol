      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23/05/11
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0022.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0022.CPv  $
      *
      *   Rev 2.4   Dec 19 2016 11:00:08   t13ex27
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.3   Nov 16 2016 08:49:02   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.2   Oct 27 2016 11:43:50   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.1   Sep 18 2012 14:00:06   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   May 23 2011 16:36:34   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0022.
           10 KNDPERSONENID                 PIC  X(0013).
           10 KNDANREDEID                   PIC S9(4) COMP.
           10 KNDANZNAME                    PIC  X(0030).
           10 KNDANZVORNAME                 PIC  X(0030).
           10 NIL-KNDANZVORNAME             PIC  X(01).
           10 KNDSUCHNAME                   PIC  X(0030).
           10 KNDSUCHVORNAME                PIC  X(0030).
           10 NIL-KNDSUCHVORNAME            PIC  X(01).
           10 KNDGEBGRUENDDAT               PIC  X(010).
           10 NIL-KNDGEBGRUENDDAT           PIC  X(01).
           10 KNDGESCHLECHT                 PIC  X(0001).
           10 KNDNUMMER                     PIC S9(9) COMP.
           10 NIL-KNDNUMMER                 PIC  X(01).
           10 KNDSTATUSID                   PIC  X(0001).
           10 KNDANZBETREUER                PIC S9(4) COMP.
           10 KNDMANDANTNR                  PIC S9(9) COMP.
           10 KNDBDSGSPERRDAT               PIC  X(010).
           10 NIL-KNDBDSGSPERRDAT           PIC  X(01).
           10 KNDNEUKUNDE                   PIC  X(010).
           10 NIL-KNDNEUKUNDE               PIC  X(01).
           10 KNDALTKUNDE                   PIC  X(010).
           10 NIL-KNDALTKUNDE               PIC  X(01).
           10 KNDWIK                        PIC  X(010).
           10 NIL-KNDWIK                    PIC  X(01).
           10 KNDGEANDERTVON                PIC  X(0004).
           10 KNDGEANDERTPGM                PIC  X(0008).
           10 NIL-KNDGEANDERTPGM            PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 KNDANTRAGSKNR                 PIC S9(9) COMP.
           10 NIL-KNDANTRAGSKNR             PIC  X(01).
 


