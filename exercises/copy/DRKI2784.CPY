      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2784.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2784.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:36:54   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:26   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:20   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:16   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:41:38   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:05:14   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:16:08   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:19:50   T13Ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aenderungs-Datei Kundendaten Commerzbank                *
      *                                                               *
      *    SATZLAENGE   300                                           *
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
      *    Vertragsnummer rechtsbündig mit führenden Nullen
           05 'XX'-LFDNR                      PIC 9(002).
      *    Mit 00 anfangen und für jeden Kontoinhaber des
      *    Vertrags laufend vergeben. Muss pro Person gleich
      *    bleiben
           05 'XX'-NACHNAME                   PIC X(030).
      *    Nachname, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-NACHNAME-KZ                PIC X(001).
      *    Kennzeichen ob sich der Nachname geaendert hat
           05 'XX'-NACHNAME-ALT               PIC X(030).
      *    Alter Nachname, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-VORNAME                    PIC X(030).
      *    Vorname, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-VORNAME-KZ                 PIC X(001).
      *    Kennzeichen ob sich der Nachname geaendert hat
           05 'XX'-VORNAME-ALT                PIC X(030).
      *    Alter Vorname, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-GEBDAT                     PIC 9(008).
      *    JJJJMMTT, Geburtsdatum des Kunden
           05 'XX'-GEBDAT-KZ                  PIC X(001).
      *    Kennzeichen ob sich der Vorname geaendert hat
           05 'XX'-ANREDE                     PIC 9(001).
      *    1=Herr, 2=Frau, 3=Firma, 9=keine Angabe
      *    Datum vom Ende des Vertrags oder der Kundenverbindung
           05 'XX'-ANREDE-KZ                  PIC X(001).
      *    Kennzeichen ob sich die Anrede geaendert hat
           05 'XX'-GESCHLECHT                 PIC 9(001).
      *    1=maennlich, 2=weiblich, 3=unbekannt
           05 'XX'-GESCHLECHT-KZ              PIC X(001).
      *    Kennzeichen ob sich das Geschlecht geaendert hat
           05 'XX'-STR-NR                     PIC X(040).
      *    Strasse und Hausnummer,
      *    linkbündig, mit Leerstellen rechts auff.
           05 'XX'-STR-NR-KZ                  PIC X(001).
      *    Kennzeichen ob sich Strasse / HausNr geaendert hat
           05 'XX'-PLZ                        PIC X(009).
      *    Postleitzahl, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-PLZ-KZ                     PIC X(001).
      *    Kennzeichen ob sich die Postleitzahl geaendert hat
           05 'XX'-ORT                        PIC X(030).
      *    Ort, linkbündig, mit Leerstellen rechts auff.
           05 'XX'-ORT-KZ                     PIC X(001).
      *    Kennzeichen ob sich der Ort geaendert hat
           05 'XX'-LAND                       PIC X(003).
      *    Laenderangabe (in ISO-Format?)
           05 'XX'-LAND-KZ                    PIC X(001).
      *    Kennzeichen ob sich das Land geaendert hat
           05 'XX'-LOESCHUNG                  PIC X(001).
      *    'D' wenn ein Kontoinhaber gelöscht wird.
      *    Es muss ein Kontoinhaber uebrig bleiben
           05 'XX'-LOESCHUNG-KZ               PIC X(001).
      *    Kennzeichen ob sich das Loesch-Kz geaendert hat
           05 FILLER                          PIC X(057).
 
 
 
 
 

