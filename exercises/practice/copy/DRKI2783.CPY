      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2783.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2783.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:36:50   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:20   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:16   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:41:32   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:05:08   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:16:02   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 04 2013 16:19:42   T13Ex77
      *Archiverstellung
      *
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - KSDS-Datei Kundendaten Commerzbank                      *
      *                                                               *
      *    SATZLAENGE   326                                           *
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
      *            Mit 00 anfangen und für jeden Kontoinhaber des
      *            Vertrags laufend vergeben. Muss pro Person gleich
      *            bleiben
               10 'XX'-NACHNAME                   PIC X(030).
      *        Nachname, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-VORNAME                    PIC X(030).
      *        Vorname, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-GEBDAT                     PIC 9(008).
      *        JJJJMMTT, Geburtsdatum des Kunden
               10 'XX'-ANREDE                     PIC 9(001).
      *        1=Herr, 2=Frau, 3=Firma, 9=keine Angabe
               10 'XX'-GESCHLECHT                 PIC 9(001).
      *        1=maennlich, 2=weiblich, 3=unbekannt
               10 'XX'-STR-NR                     PIC X(040).
      *        Strasse und Hausnummer,
      *        linkbündig, mit Leerstellen rechts auff.
               10 'XX'-PLZ                        PIC X(009).
      *        Postleitzahl, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-ORT                        PIC X(030).
      *        Ort, linkbündig, mit Leerstellen rechts auff.
               10 'XX'-LAND                       PIC X(003).
      *        Laenderangabe (in ISO-Format?)
               10 'XX'-LOESCHUNG                  PIC X(001).
      *        'D' wenn ein Kontoinhaber gelöscht wird.
      *        Es muss ein Kontoinhaber uebrig bleiben
               10 FILLER                          PIC X(127).
      *---------------------------------------------------------------*
 
 
 
 
 

