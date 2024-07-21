      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0438.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/WUKI0438.CPv  $
      *
      *   Rev 2.4   Nov 24 2010 10:33:38   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.3   Oct 26 2010 10:51:52   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 01 2010 11:59:34   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   02 Dec 2008 14:28:30   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.0   02 Dec 2008 13:48:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-REISEGEPAECKVERT.
           10 RGPVERTRAGSID                 PIC  X(0013).
           10 RGPVETLFDNR                   PIC S9(4) COMP.
           10 RGPEINZVTGHOSTID              PIC  X(0015).
           10 RGPBEDINGUNG-KTY              PIC S9(4) COMP.
           10 RGPBEDINGUNG-KNR              PIC S9(15)V COMP-3.
           10 RGPVERSSUMME                  PIC S9(9) COMP.
           10 RGPMAILING-KTY                PIC S9(4) COMP.
           10 RGPMAILING-KNR                PIC S9(15)V COMP-3.
           10 RGPZUSINFO-KTY                PIC S9(4) COMP.
           10 RGPZUSINFO-KNR                PIC S9(15)V COMP-3.
           10 RGPDIFDECKKZ                  PIC  X(0001).
           10 NIL-RGPDIFDECKKZ              PIC  X(01).
           10 RGPGESELLSCH-KTY              PIC S9(4) COMP.
           10 RGPGESELLSCH-KNR              PIC S9(15)V COMP-3.
           10 RGPVERSNUMMER                 PIC  X(0040).
           10 NIL-RGPVERSNUMMER             PIC  X(01).
           10 RGPVUMSTELLUNGDAT             PIC  X(010).
           10 NIL-RGPVUMSTELLUNGDAT         PIC  X(01).
           10 RGPKUENDIGUNGKZ               PIC  X(0001).
           10 NIL-RGPKUENDIGUNGKZ           PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
