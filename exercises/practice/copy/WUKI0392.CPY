      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05/12/11
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0392.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0392.CPv  $
      *
      *   Rev 2.1   Feb 03 2012 15:16:36   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.0   Dec 5 2011 15:12:13   T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-UNFBEITRAGRUECK.
           10 UBRVERTRAGSID                 PIC  X(0013).
           10 UBRVETLFDNR                   PIC S9(4) COMP.
           10 UBRRGID                       PIC S9(4) COMP.
           10 UBRJAHR                       PIC S9(4) COMP.
           10 UBRMONAT                      PIC S9(4) COMP.
           10 UBRMONNR                      PIC S9(4) COMP.
           10 NIL-UBRMONNR                  PIC  X(01).
           10 UBRLFDMON                     PIC S9(4) COMP.
           10 NIL-UBRLFDMON                 PIC  X(01).
           10 UBRSTATUS                     PIC  X(0001).
           10 NIL-UBRSTATUS                 PIC  X(01).
           10 UBRERSTDATUM                  PIC  X(010).
           10 NIL-UBRERSTDATUM              PIC  X(01).
           10 UBRGARRUEWERT                 PIC S9(11)V9(4) COMP-3.
           10 NIL-UBRGARRUEWERT             PIC  X(01).
           10 UBRGARTODLEIST                PIC S9(11)V9(4) COMP-3.
           10 NIL-UBRGARTODLEIST            PIC  X(01).
           10 UBRRUEWERTUEB                 PIC S9(11)V9(4) COMP-3.
           10 NIL-UBRRUEWERTUEB             PIC  X(01).
           10 UBRTODLEIUEB                  PIC S9(11)V9(4) COMP-3.
           10 NIL-UBRTODLEIUEB              PIC  X(01).
           10 UBRSTILLERES                  PIC S9(11)V9(4) COMP-3.
           10 NIL-UBRSTILLERES              PIC  X(01).
           10 UBRRECHZINS                   PIC S9(2)V9(2) COMP-3.
           10 NIL-UBRRECHZINS               PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
