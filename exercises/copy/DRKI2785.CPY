      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2785.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2785.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:36:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:30   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:24   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:20   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:41:44   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:05:20   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:16:14   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:19:56   T13Ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - KSDS-Datei Baufi-Daten Commerzbank                      *
      *                                                               *
      *    SATZLAENGE   526                                           *
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
 
           05  'XX'-TIMESTAMP                     PIC X(026).
           05  'XX'-DATEN-SATZ.
               10 'XX'-SATZART                    PIC X(001).
      *           Inhalt C
               10 'XX'-KEY.
                   15 'XX'-GESELLSCHAFT           PIC 9(002).
      *            2-stellige DVAG interne Nummer für die Gesellschaft
                   15 'XX'-VERTRAGSNUMMER         PIC 9(015).
      *            Vertragsnummer rechtsbündig mit führenden Nullen
                   15 'XX'-LFDNR                  PIC 9(002).
      *            Mit 00 anfangen und für jeden Baufi-Satz des
      *            Vertrags laufend vergeben.
               10 'XX'-PROD-BEG-DAT               PIC 9(008).
      *        JJJJMMTT  Datum der Eroeffnung des Produkts
               10 'XX'-PROD-ABL-DAT               PIC 9(008).
      *        JJJJMMTT  Datum der Schliessung des Produkts
               10 'XX'-BETRAG-ANF                 PIC 9(12)V99.
      *        Ursprungskreditbetrag
               10 'XX'-FZ-ENDE-DAT                PIC 9(008).
      *        JJJJMMTT  Ende der Zinsfestschreibung
               10 'XX'-BETR-AKT                   PIC 9(12)V99.
      *        Aktueller Darlehensbetrag
               10 'XX'-TILG-START                 PIC 9(008).
      *        JJJJMMTT  Beginn der Tilgung
               10 'XX'-TILGUNGSART                PIC X(001).
      *        A=Annuitaetisch, E=Endfaellig
               10 'XX'-TILG-PROZENT               PIC 9(02)V9999.
      *        Anfaengliche Tilgung
               10 'XX'-NOM-PROZENT                PIC 9(02)V9999.
      *        Nominalzinssatz
               10 'XX'-ZAHLUNGSART                PIC 9(001).
      *        0=Zahlweise fehlt, 1=jaehrlich, 2=halbjaehrlich,
      *        4=vierteljaehrlich, 5=2-monatlich, 6=monatlich,
      *        8=Einmalzahlung
               10 'XX'-RATE                       PIC 9(12)V99.
      *        Rueckzahlrate
               10 'XX'-ZUSAGE-DAT                 PIC 9(008).
      *        JJJJMMTT  Datum der Kreditzusage
               10 'XX'-AUSZAHL-DAT                PIC 9(008).
      *        JJJJMMTT  Datum der Kreditauszahlung
               10 'XX'-DARLART                    PIC X(001).
      *        z.B. A=KfW-Darlehen, B=Zwischenfinanzierung,
      *        C=Forward-Darlehen, D=Immobiliendarlehen
               10 'XX'-EINZUG-IBAN                PIC X(034).
      *        IBAN zur Lastschrift für den Rateneinzug
               10 'XX'-EINZUG-BIC                 PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug
               10 'XX'-MANDAT-REF                 PIC X(035).
      *        Mandat Referanznummer
               10 'XX'-ABWEICH-BZ                 PIC X(001).
      *        'J' = Es gibt einen abweichenden Beitragszahler.
      *        'N' = Beitragszahler ist der Kontoinhaber.
               10 'XX'-NACHNAME-BZ                PIC X(030).
      *        Nachname, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-VORNAME-BZ                 PIC X(030).
      *        Vorname, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-GEBDAT-BZ                  PIC 9(008).
      *        JJJJMMTT, Geburtsdatum des Kunden
               10 'XX'-ANREDE-BZ                  PIC 9(001).
      *        1=Herr, 2=Frau, 3=Firma, 9=keine Angabe
               10 'XX'-STR-NR-BZ                  PIC X(040).
      *        Strasse und Hausnummer,
      *        linkbündig, mit Leerstellen rechts auff.
               10 'XX'-PLZ-BZ                     PIC X(009).
      *        Postleitzahl, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-ORT-BZ                     PIC X(030).
      *        Ort, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-LAND-BZ                    PIC X(003).
      *        Laenderangabe (in ISO-Format?)
               10 FILLER                          PIC X(143).
 
 
 
 
 

