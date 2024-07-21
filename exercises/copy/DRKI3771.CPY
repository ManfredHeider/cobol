      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3771.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI3771.CPv  $
      *
      *   Rev 2.8   Oct 08 2020 14:04:58   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.7   Oct 08 2020 13:59:02   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.6   Oct 08 2020 13:33:32   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.5   Oct 08 2020 13:11:44   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.4   Oct 06 2020 17:07:06   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.3   Oct 06 2020 12:43:36   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.2   Oct 06 2020 12:26:16   u130684
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.1   Jun 23 2020 12:23:40   t13ex27
      *AKI01773: Anbindung Compeon an DV - DTA-Kontrolle
      *
      *   Rev 2.0   Apr 03 2020 09:03:14   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *
      *
       02  DRKI3771.

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
      * Kundennummer
               20  'XX'-KNR              PIC X(015).
      * Rechtsform
               20  'XX'-RECHTSFORM       PIC 9(002).
      * Anrede
               20  'XX'-ANREDE           PIC 9(001).
      * Vorname der Firma, z. B. bei Einzelunternehmen
               20  'XX'-FIRM-VNAME       PIC X(030).
      * Nachname bei Einzelunternehmen
               20  'XX'-FIRM-NNAME       PIC X(030).
      * Gründungsdatum des Unternehmens
               20  'XX'-GRUENDDAT        PIC 9(008).
      * Firmeninhaber
               20  'XX'-FIRM-INH         PIC X(100).
      * Straße
               20  'XX'-STR              PIC X(040).
      * Postleitzahl
               20  'XX'-PLZ              PIC X(009).
      * Ort
               20  'XX'-ORT              PIC X(040).
      * Postfach
               20  'XX'-PF               PIC X(009).
      * Postleitzahl des Postfachs
               20  'XX'-PF-PLZ           PIC X(009).
      * Ort des Postfachs
               20  'XX'-PF-ORT           PIC X(040).
      * Telefonnummer der Firma
               20  'XX'-FON              PIC X(030).
      * Mobilfunknummer der Firma
               20  'XX'-MOBIL            PIC X(030).
      * Faxnummer der Firma
               20  'XX'-FAX              PIC X(030).
      * Titel des Ansprechpartners
               20  'XX'-AP-TITEL         PIC X(002).
      * Anrede des Ansprechpartners
               20  'XX'-AP-ANREDE        PIC X(001).
      * Vorname des Ansprechpartners
               20  'XX'-AP-VNAME         PIC X(030).
      * Nachname des Ansprechpartners
               20  'XX'-AP-NNAME         PIC X(030).
      * Geburtsdatum des Ansprechpartners
               20  'XX'-AP-GEBDAT        PIC 9(008).
      * Emailadresse
               20  'XX'-AP-EMAIL         PIC X(128).
      * Funktion des Ansprechpartners
               20  'XX'-AP-FKT           PIC X(050).
      *
               20  FILLER                PIC X(309).
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
      * Empfänger je nach Mandant ('DVAG', 'ALLFL, 'ALAG')
               20  'XX'-EMPFAENGER       PIC X(010).
      * Inhaltsangabe
               20  'XX'-INHALT           PIC X(020).
      * Art der Lieferung
               20  'XX'-ART              PIC X(001).
      * Reserve
               20  FILLER                PIC X(956).
      *
      ***********************************************************
      *                                                         *
      *                 NACHSATZ                                *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ  REDEFINES      'XX'-SATZ.
      *
      * Satzart = 'E'
               20  'XX'-N-SA             PIC X(001).
      * Anzahl der gelieferten D-Sätze
               20  'XX'-ANZ              PIC 9(008).
      * Reserve
               20  FILLER                PIC X(990).







