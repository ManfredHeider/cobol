      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0544.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0544.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:41:48   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:04:40   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0544.
           10 AKOANGEBOTSNAME               PIC  X(0010).
           10 AKOVERTRAGSID                 PIC  X(0013).
           10 AKOPERSONLFDNR                PIC S9(4) COMP.
           10 AKOTARIFTEIL-ID               PIC  X(0010).
           10 AKOSTICHTAG                   PIC  X(010).
           10 AKOTARIFNAME                  PIC  X(0040).
           10 AKOGESAMT-BEITRAG             PIC S9(7)V9(2) COMP-3.
           10 AKOSELBSTBEHALT               PIC S9(7)V9(2) COMP-3.
           10 AKOVERS-LEISTUNG              PIC S9(7)V9(2) COMP-3.
           10 AKOGESETZL-ZUSCHL             PIC S9(7)V9(2) COMP-3.
           10 AKOZEITL-BEFR-LIM             PIC S9(7)V9(2) COMP-3.
           10 AKOSUM-BEITRZUSCHL            PIC S9(7)V9(2) COMP-3.
           10 AKOPAUSCH-LEIST-1             PIC S9(7)V9(2) COMP-3.
           10 AKOPAUSCH-LEIST-2             PIC S9(7)V9(2) COMP-3.
           10 AKOPAUSCH-LEIST-3             PIC S9(7)V9(2) COMP-3.
           10 AKOPAUSCH-LEIST-4             PIC S9(7)V9(2) COMP-3.
           10 LETZTEAENDERUNG               PIC  X(0026).

