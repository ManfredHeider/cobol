      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0545.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0545.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:42:30   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:06:37   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0545.
           10 AKMANGEBOTSNAME               PIC  X(0010).
           10 AKMVERTRAGSID                 PIC  X(0013).
           10 AKMPERSONLFDNR                PIC S9(4) COMP.
           10 AKMTARIFTEIL-ID               PIC  X(0010).
           10 AKMANGEBOTSNR                 PIC S9(4) COMP.
           10 AKMSTICHTAG                   PIC  X(010).
           10 AKMTARIFNAME                  PIC  X(0040).
           10 AKMTARIFSTATUS                PIC  X(0020).
           10 AKMGESAMT-BEITRAG             PIC S9(7)V9(2) COMP-3.
           10 AKMSELBSTBEHALT               PIC S9(7)V9(2) COMP-3.
           10 AKMVERS-LEISTUNG              PIC S9(7)V9(2) COMP-3.
           10 AKMGESETZL-ZUSCHL             PIC S9(7)V9(2) COMP-3.
           10 AKMZEITL-BEFR-LIM             PIC S9(7)V9(2) COMP-3.
           10 AKMSUM-BEITRZUSCHL            PIC S9(7)V9(2) COMP-3.
           10 AKMPAUSCH-LEIST-1             PIC S9(7)V9(2) COMP-3.
           10 AKMPAUSCH-LEIST-2             PIC S9(7)V9(2) COMP-3.
           10 AKMPAUSCH-LEIST-3             PIC S9(7)V9(2) COMP-3.
           10 AKMPAUSCH-LEIST-4             PIC S9(7)V9(2) COMP-3.
           10 LETZTEAENDERUNG               PIC  X(0026).

