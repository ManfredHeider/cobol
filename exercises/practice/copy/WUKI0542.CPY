      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0542.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0542.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:40:30   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 14:44:58   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0542.
           10 AKVANGEBOTSNAME               PIC  X(0010).
           10 AKVVERTRAGSID                 PIC  X(0013).
           10 AKVVERTRAGSNUMMER             PIC  X(0015).
           10 AKVUMST-TERMIN                PIC  X(010).
           10 AKVNACHNAME-VN                PIC  X(0040).
           10 AKVVORNAME-VN                 PIC  X(0030).
           10 AKVANZ-VP                     PIC S9(4) COMP.
           10 AKVANNAHME-BIS                PIC  X(010).
           10 AKVAUFBEWAHR-BIS              PIC  X(010).
           10 LETZTEAENDERUNG               PIC  X(0026).

