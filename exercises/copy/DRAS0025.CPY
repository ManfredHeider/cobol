      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0025.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAS0025.CPv  $
      *
      *   Rev 2.3   Jun 04 2020 09:40:28   t13ae50
      *AAS01341: 2020 - Jetzt erst recht: Neukunden/aktivierte Einv
      *ertragskunden - Ermittlung (VB-System)
      *
      *   Rev 2.2   Jun 03 2009 15:41:42   t13ae36
      *AAS00815: Neukundenwettbewerb
      *
      *   Rev 2.1   Apr 27 2009 10:04:54   t13ae36
      *AAS00815: Neukundenwettbewerb
      *
      *   Rev 2.0   Apr 08 2009 09:37:18   t13ae36
      *Archiverstellung
      *
      *   Eingangsschnittstelle (Neukunden Wettbewerb)
      *                         Vertragsdaten aus Aachen
      *---------------------------------------------------------------*
       01   DRAS0025.
      **    ProduktgeberCode
            05 DRAS0025-PROD-GEBER                 PIC 9(02).
      **    Produktgeber-Bezeichnung
            05 DRAS0025-PROD-GEBER-BEZ             PIC X(10).
      **    Vertragsnummer des Produktgebers
            05 DRAS0025-VNR-ABR                    PIC 9(15).
      **    VB-NR des vermittelnden VBs
            05 DRAS0025-MA-ST-NR                   PIC 9(07).
      **    Produktschlüssel
            05 DRAS0025-PROD-SCHL                  PIC 9(03).
      **    Produktart
            05 DRAS0025-PROD-ART                   PIC 9(02).
      **    neue Eigeneinheiten des VBs für den Vertrag
            05 DRAS0025-EH-EIGNEU                  PIC S9(11) COMP-3.
      **    Buchungsdatum
            05 DRAS0025-MONAT                      PIC X(10).
      **    Eindeutiges Schlüsselfeld (aus Timestamp)
            05 DRAS0025-ABR-ID                     PIC S9(15) COMP-3.
      **    Attribut Produkt-Sparte
            05 DRAS0025-PROD-SPARTE                PIC 9(03).

