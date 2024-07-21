      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0026.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAS0026.CPv  $
      *
      *   Rev 2.3   Jun 04 2020 09:40:50   t13ae50
      *AAS01341: 2020 - Jetzt erst recht: Neukunden/aktivierte Einv
      *ertragskunden - Ermittlung (VB-System)
      *
      *   Rev 2.2   Jun 03 2009 15:41:52   t13ae36
      *AAS00815: Neukundenwettbewerb
      *
      *   Rev 2.1   Apr 27 2009 10:04:58   t13ae36
      *AAS00815: Neukundenwettbewerb
      *
      *   Rev 2.0   Apr 08 2009 09:37:24   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *   Ausgangsschnittstelle (Neukunden Wettbewerb)
      *                         Vertragsdaten aus Aachen
      *---------------------------------------------------------------*
       01   DRAS0026.
      **    ProduktgeberCode
            05 DRAS0026-PROD-GEBER                 PIC 9(02).
      **    Produktgeber-Bezeichnung
            05 DRAS0026-PROD-GEBER-BEZ             PIC X(10).
      **    Vertragsnummer des Produktgebers
            05 DRAS0026-VNR-ABR                    PIC 9(15).
      **    VB-NR des vermittelnden VBs
            05 DRAS0026-MA-ST-NR                   PIC 9(07).
      **    Produktschlüssel
            05 DRAS0026-PROD-SCHL                  PIC 9(03).
      **    Produktart
            05 DRAS0026-PROD-ART                   PIC 9(02).
      **    neue Eigeneinheiten des VBs für den Vertrag
            05 DRAS0026-EH-EIGNEU                  PIC S9(11) COMP-3.
      **    Buchungsdatum
            05 DRAS0026-MONAT                      PIC X(10).
      **    Eindeutiges Schlüsselfeld (aus Timestamp)
            05 DRAS0026-ABR-ID                     PIC S9(15) COMP-3.
      **    Attribut Produkt-Sparte
            05 DRAS0026-PROD-SPARTE                PIC 9(03).
      **    ermittelte Vertragsnummer (KI)
            05 DRAS0026-VNR-KI                     PIC 9(15).
 

