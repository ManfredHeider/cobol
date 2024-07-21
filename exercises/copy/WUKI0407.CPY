      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10/06/10
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0407.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0407.CPv  $
      *
      *   Rev 2.4   Jun 17 2020 10:16:10   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.3   Jun 23 2016 09:21:56   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP
      *
      *   Rev 2.2   Feb 17 2011 14:26:34   t13ex27
      *AKI01022: DB-Änderungen für R. 5.28
      *
      *   Rev 2.1   Jun 28 2010 10:14:48   t13ex48
      *AKI00970: DB-Änderungen für Rel. 5.12: SUH
      *
      *   Rev 2.0   Jun 10 2010 14:51:52   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0407.
           10 VEZVERTRAGSID                 PIC  X(0013).
           10 VEZVETLFDNR                   PIC S9(4) COMP.
           10 VEZHOSTID                     PIC  X(0015).
           10 VEZSPARTE-KTY                 PIC S9(4) COMP.
           10 VEZSPARTE-KNR                 PIC S9(15)V COMP-3.
           10 VEZTARPROD-KTY                PIC S9(4) COMP.
           10 VEZTARPROD-KNR                PIC S9(15)V COMP-3.
           10 VEZBEGINN                     PIC  X(010).
           10 NIL-VEZBEGINN                 PIC  X(01).
           10 VEZABLAUF                     PIC  X(010).
           10 NIL-VEZABLAUF                 PIC  X(01).
           10 VEZSTATUS                     PIC S9(4) COMP.
           10 VEZBBEITRAGZAHLW              PIC S9(8)V9(2) COMP-3.
           10 VEZSUMME                      PIC S9(10)V COMP-3.
           10 VEZVERTRARTEZV                PIC S9(9) COMP.
           10 VEZAENDERUNGSGRD              PIC  X(0050).
           10 VEZAENDDAT                    PIC  X(010).
           10 NIL-VEZAENDDAT                PIC  X(01).
           10 VEZNJBEITRAG                  PIC S9(8)V9(2) COMP-3.
           10 VEZBJBEITRAG                  PIC S9(8)V9(2) COMP-3.
           10 VEZNBEITRAGZAHLW              PIC S9(8)V9(2) COMP-3.
           10 VEZBEARBEITDAT                PIC  X(010).
           10 NIL-VEZBEARBEITDAT            PIC  X(01).
           10 VEZGESCHAEFTSZW               PIC  X(0002).
           10 VEZPRODUKTID                  PIC S9(4) COMP.
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 VEZAENDGRUND-KTY              PIC S9(4) COMP.
           10 VEZAENDGRUND-KNR              PIC S9(15)V COMP-3.
           10 VEZRABNJBEITRAG               PIC S9(8)V9(2) COMP-3.
           10 NIL-VEZRABNJBEITRAG           PIC  X(01).
           10 VEZSUMKOMPBEITR               PIC S9(8)V9(2) COMP-3.
           10 NIL-VEZSUMKOMPBEITR           PIC  X(01).
           10 VEZREOBPRODNR                 PIC S9(9) COMP.
           10 NIL-VEZREOBPRODNR             PIC  X(01).
           10 VEZPRODBEZ-KTY                PIC S9(4) COMP.
           10 VEZPRODBEZ-KNR                PIC S9(15)V COMP-3.
           10 NIL-VEZPRODBEZ-KNR            PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 VEZFLEXNSP                    PIC S9(9)V9(2) COMP-3.
           10 NIL-VEZFLEXNSP                PIC  X(01).
           10 VEZFLEXNSPGW                  PIC  X(0001).
           10 NIL-VEZFLEXNSPGW              PIC  X(01).

