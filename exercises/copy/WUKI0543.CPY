      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0543.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0543.CPv  $
      *
      *   Rev 2.2   Jul 31 2020 14:00:16   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.1   Jul 01 2020 09:41:08   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:02:34   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0543.
           10 AKPANGEBOTSNAME               PIC  X(0010).
           10 AKPVERTRAGSID                 PIC  X(0013).
           10 AKPPERSONLFDNR                PIC S9(4) COMP.
           10 AKPUMST-TERMIN                PIC  X(010).
           10 AKPNACHNAME-VP                PIC  X(0040).
           10 AKPVORNAME-VP                 PIC  X(0030).
           10 AKPGEBDAT-VP                  PIC  X(010).
           10 AKPANZ-ANGEBOTE               PIC S9(4) COMP.
           10 LETZTEAENDERUNG               PIC  X(0026).

