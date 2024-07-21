      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2786.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2786.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:37:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:34   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:28   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:24   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:41:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:05:24   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:16:22   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:20:02   T13Ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aenderungs-Datei Baufi-Daten Commerzbank                *
      *                                                               *
      *    SATZLAENGE   500                                           *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      *  AENDERUNGEN:                                                 *
      *  AUFTRAG !  DATUM   ! VER ! AENDERUNG              ! NAME     *
      * ------------------------------------------------------------- *
      * AKI01251 ! 04.09.13 !  01 ! Neuerstellung          ! Borgmann *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *          !          !     !                        !          *
      *  ENDE-LOGBUCH                                                 *
      *****************************************************************
 
           05 'XX'-VERARB-KZ                  PIC X(001).
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-GESELLSCHAFT               PIC 9(002).
      *    2-stellige DVAG interne Nummer für die Gesellschaft
           05 'XX'-VERTRAGSNUMMER             PIC 9(015).
      *    Laufende Nummer
           05 'XX'-LFDNR                      PIC 9(002).
      *    Vertragsnummer rechtsbündig mit führenden Nullen
           05 'XX'-PROD-BEG-DAT               PIC 9(008).
      *    JJJJMMTT  Datum der Eroeffnung des Produkts
           05 'XX'-PROD-BEG-DAT-KZ            PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-PROD-ABL-DAT               PIC 9(008).
      *    JJJJMMTT  Datum der Schliessung des Produkts
           05 'XX'-PROD-ABL-DAT-KZ            PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-BETRAG-ANF                 PIC 9(12)V99.
      *    Ursprungskreditbetrag
           05 'XX'-BETRAG-ANF-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-FZ-ENDE-DAT                PIC 9(008).
      *    JJJJMMTT  Ende der Zinsfestschreibung
           05 'XX'-FZ-ENDE-DAT-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-BETR-AKT                   PIC 9(12)V99.
      *    Aktueller Darlehensbetrag
           05 'XX'-BETR-AKT-KZ                PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-TILG-START                 PIC 9(008).
      *    JJJJMMTT  Beginn der Tilgung
           05 'XX'-TILG-START-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-TILGUNGSART                PIC X(001).
      *    A=Annuitaetisch, E=Endfaellig
           05 'XX'-TILGUNGSART-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-TILG-PROZENT               PIC 9(02)V9999.
      *    Anfaengliche Tilgung
           05 'XX'-TILG-PROZENT-KZ            PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-NOM-PROZENT                PIC 9(02)V9999.
      *    Nominalzinssatz
           05 'XX'-NOM-PROZENT-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-ZAHLUNGSART                PIC 9(001).
      *    0=Zahlweise fehlt, 1=jaehrlich, 2=halbjaehrlich,
      *    4=vierteljaehrlich, 5=2-monatlich, 6=monatlich,
      *    8=Einmalzahlung
           05 'XX'-ZAHLUNGSART-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-RATE                       PIC 9(12)V99.
      *    Rueckzahlrate
           05 'XX'-RATE-KZ                    PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-ZUSAGE-DAT                 PIC 9(008).
      *    JJJJMMTT  Datum der Kreditzusage
           05 'XX'-ZUSAGE-DAT-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-AUSZAHL-DAT                PIC 9(008).
      *    JJJJMMTT  Datum der Kreditauszahlung
           05 'XX'-AUSZAHL-DAT-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-DARLART                    PIC X(001).
      *    z.B. A=KfW-Darlehen, B=Zwischenfinanzierung,
      *    C=Forward-Darlehen, D=Immobiliendarlehen
           05 'XX'-DARLART-KZ                 PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-EINZUG-IBAN                PIC X(034).
      *    IBAN zur Lastschrift für den Rateneinzug
           05 'XX'-EINZUG-IBAN-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-EINZUG-BIC                 PIC X(011).
      *    BIC zur Lastschrift für den Rateneinzug
           05 'XX'-EINZUG-BIC-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-MANDAT-REF                 PIC X(035).
      *    Mandat Referanznummer
           05 'XX'-MANDAT-REF-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-ABWEICH-BZ                 PIC X(001).
      *    'J' = Es gibt einen abweichenden Beitragszahler.
      *    'N' = Beitragszahler ist der Kontoinhaber.
           05 'XX'-ABWEICH-BZ-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-NACHNAME-BZ                PIC X(030).
      *    Nachname, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-NACHNAME-BZ-KZ             PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-VORNAME-BZ                 PIC X(030).
      *    Vorname, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-VORNAME-BZ-KZ              PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-GEBDAT-BZ                  PIC 9(008).
      *    JJJJMMTT, Geburtsdatum des Kunden
           05 'XX'-GEBDAT-BZ-KZ               PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-ANREDE-BZ                  PIC 9(001).
      *    1=Herr, 2=Frau, 3=Firma, 9=keine Angabe
           05 'XX'-ANREDE-BZ-KZ               PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-STR-NR-BZ                  PIC X(040).
      *    Strasse und Hausnummer,
      *    linkbündig, mit Leerstellen rechts auff.
           05 'XX'-STR-NR-BZ-KZ               PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-PLZ-BZ                     PIC X(009).
      *    Postleitzahl, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-PLZ-BZ-KZ                  PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-ORT-BZ                     PIC X(030).
      *    Ort, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-ORT-BZ-KZ                  PIC X(001).
      *    Aenderungskennzeichen
           05 'XX'-LAND-BZ                    PIC X(003).
      *    Laenderangabe (in ISO-Format?)
           05 'XX'-LAND-BZ-KZ                 PIC X(001).
      *    Aenderungskennzeichen
           05 FILLER                          PIC X(117).
 
 
 
 
 

