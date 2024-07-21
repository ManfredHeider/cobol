      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3770.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI3770.CPv  $
      *
      *   Rev 2.8   Oct 08 2020 14:04:52   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.7   Oct 08 2020 13:58:56   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.6   Oct 08 2020 13:33:26   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.5   Oct 08 2020 13:11:38   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.4   Oct 06 2020 17:06:58   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.3   Oct 06 2020 12:43:30   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.2   Oct 06 2020 12:26:10   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.1   Jun 23 2020 12:22:50   t13ex27
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.0   Apr 03 2020 09:02:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *
      *
       02  DRKI3770.

      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ.
      *
      * Satzart = 'D'
               20  'XX'-SA               PIC X(001).
      * Partnergesellschaft
               20  'XX'-GES              PIC 9(002).
      * Vertragsnummer
               20  'XX'-VNR              PIC X(015).
      * VB-Nummer
               20  'XX'-VBNR             PIC 9(007).
      * Datum Eröffnung JJJJMMTT
               20  'XX'-VEROEFF          PIC 9(008).
      * Datum Schließung JJJJMMTT
               20  'XX'-SCHLIESS         PIC 9(008).
      * Finanzierungsbetrag
               20  'XX'-FINBETR          PIC 9(012)V99.
      * Zusagedatum JJJJMMTT
               20  'XX'-ZUSAGE           PIC 9(008).
      * Datum der Auszahlung JJJJMMTT
               20  'XX'-AUSZAHL          PIC 9(008).
      * Name des Finanzierungspartners
               20  'XX'-PRODUKTGEBER     PIC X(030).
      * Produktart - Leasing, Mietkauf oder Darlehen
               20  'XX'-PRODUKTART       PIC X(001).
      * Aktueller Finanzierungsbetrag
               20  'XX'-AKTSALDO         PIC 9(012)V99.
      * Sollzinssatz p. a.
               20  'XX'-SOLLZINS         PIC 9(002)V9999.
      * Rate
               20  'XX'-RATE             PIC 9(012)V99.
      * Datum Fälligkeit der nächsten Rate JJJJMMTT
               20  'XX'-FAELLIG          PIC 9(008).
      * Restlaufzeit in Monaten
               20  'XX'-REST             PIC 9(003).
      * Delete-Kennzeichen
               20  'XX'-AKTUAL-KZ        PIC X(001).
      * International Bank Account Number(Belastungskonto)
               20  'XX'-IBAN             PIC X(034).
      * Business Identifier Code(Belastungskonto)
               20  'XX'-BIC              PIC X(011).
      * Mandatsreferenz
               20  'XX'-MANDAT           PIC X(035).
      * Nachname bei Einzelunternehmen sonst Firmenname
               20  'XX'-KTO-INH-NNAME    PIC X(030).
      * Vorname bei Einzelunternehmen
               20  'XX'-KTO-INH-VNAME    PIC X(030).
      * International Bank Account Number(Kreditkonto)
               20  'XX'-IBAN-KREDIT      PIC X(034).
      * Reserve
               20  FILLER                PIC X(034).
      *
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-VORSATZ  REDEFINES      'XX'-SATZ.
      *
      * Satzart = 'A' für Vorsatz
               20  'XX'-V-SA             PIC X(001).
      * Absender = '90' für Compeon
               20  'XX'-ABSENDER         PIC 9(002).
      * Erfassungs-/Änderungsdatum des DTA im Format JJJJMMTT
               20  'XX'-DATUM            PIC 9(008).
      * Empfänger je nach Mandant ('DVAG', 'ALLF', 'ALAG')
               20  'XX'-EMPFAENGER       PIC X(004).
      * Art der Lieferung
               20  'XX'-ART              PIC X(008).
      * Reserve
               20  FILLER                PIC X(333).
      *
      ***********************************************************
      *                                                         *
      *                 NACHSATZ                                *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ  REDEFINES     'XX'-SATZ.
      *
      * Satzart = 'E'
               20  'XX'-N-SA             PIC X(001).
      * Anzahl der gelieferten D-Sätze
               20  'XX'-ANZ              PIC 9(008).
      * Reserve
               20  FILLER                PIC X(347).







