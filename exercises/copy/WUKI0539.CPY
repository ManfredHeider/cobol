      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/04/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0539.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0539.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:38:52   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 12:01:46   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:14:49   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0539.
           10 DCOVERTRAGSID                 PIC  X(0013).
           10 DCOPRODUKTID                  PIC S9(4) COMP.
           10 DCOPRODUKTART                 PIC  X(0001).
           10 DCOFINBETRAG                  PIC S9(12)V9(2) COMP-3.
           10 DCOZUSAGE                     PIC  X(010).
           10 NIL-DCOZUSAGE                 PIC  X(01).
           10 DCOAUSZAHLUNG                 PIC  X(010).
           10 NIL-DCOAUSZAHLUNG             PIC  X(01).
           10 DCOPRODUKTGEBER               PIC  X(0030).
           10 NIL-DCOPRODUKTGEBER           PIC  X(01).
           10 DCOAKTSALDO                   PIC S9(12)V9(2) COMP-3.
           10 NIL-DCOAKTSALDO               PIC  X(01).
           10 DCOSOLLZINS                   PIC S9(2)V9(4) COMP-3.
           10 DCORATE                       PIC S9(12)V9(2) COMP-3.
           10 DCOFAELLIG                    PIC  X(010).
           10 NIL-DCOFAELLIG                PIC  X(01).
           10 DCORESTLAUF                   PIC S9(3)V COMP-3.
           10 DCOIBANKREDIT                 PIC  X(0034).
           10 NIL-DCOIBANKREDIT             PIC  X(01).
           10 DCODELETEKZ                   PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).

