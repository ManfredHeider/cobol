      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUOK0007.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUOK0007.CPv  $
      *
      *   Rev 2.2   Feb 18 2014 16:22:30   t13ex19
      *FKI01448: Anpassung VN-Wechsel für Cloud
      *
      *   Rev 2.1   Nov 22 2012 14:59:32   t13ex27
      *AOK00051: Projekt Aurora - Bestandsfunktionen-Abgleich
      *
      *   Rev 2.0   Sep 24 2012 11:57:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-HISTNAMENAENDERUNG.
           10 HNAKUNDENID                   PIC  X(0013).
           10 HNALFDNR                      PIC S9(4) COMP.
           10 HNAERSTELLTVON                PIC  X(0030).
           10 NIL-HNAERSTELLTVON            PIC  X(01).
           10 HNAPARTGESELLSCHID            PIC S9(4) COMP.
           10 NIL-HNAPARTGESELLSCHID        PIC  X(01).
           10 HNAVERTRAGSNUMMER             PIC  X(0015).
           10 NIL-HNAVERTRAGSNUMMER         PIC  X(01).
           10 HNAERSTELLT                   PIC  X(026).
           10 HNATICKETID                   PIC S9(18)V COMP-3.
           10 LETZTEAENDERUNG               PIC  X(026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 HNA-KTY                       PIC S9(4) COMP.
           10 HNA-KNR                       PIC S9(4) COMP.
           10 HNAALTANREDEID                PIC S9(4) COMP.
           10 HNAALTVORNAME                 PIC  X(0030).
           10 HNAALTNACHNAME                PIC  X(0030).
           10 HNAALTGEBGRUENDDAT            PIC  X(010).
           10 HNANEUANREDEID                PIC S9(4) COMP.
           10 HNANEUVORNAME                 PIC  X(0030).
           10 HNANEUNACHNAME                PIC  X(0030).
           10 HNANEUGEBGRUENDDAT            PIC  X(010).
           10 HNANEUKUNDENID                PIC  X(0013).
           10 NIL-HNANEUKUNDENID            PIC  X(01).
