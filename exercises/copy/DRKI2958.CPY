      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2958.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2958.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 08:19:56   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:01:50   t13ex73
      *AKI01342: DVM G90 Erstellung - MDTA Copystrecken
      *
      *   Rev 2.1   Jul 09 2015 10:38:04   t13ex73
      *AKI01342: DVM G90 Erstellung - MDTA Copystrecken
      *
      *   Rev 2.0   Jan 19 2015 09:28:26   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 5 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DV  Kundendaten DTA                      *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  1000 Bytes                                      *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI2958.
      *
      *    A fuer Vorsatz, D fuer Datensatz und E fuer Nachsatz
           05  'XX'-SA                      PIC  X(001).
               88 'XX'-SATZART-VORSATZ          VALUE 'A'.
               88 'XX'-SATZART-DATENSATZ        VALUE 'D'.
               88 'XX'-SATZART-ENDESATZ         VALUE 'E'.
      *
      *
           05  'XX'-DATENSATZ.
      *
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
               10 'XX'-D-GES                PIC  9(002).
      *
      *        Die Vertragsnummer mit fuehrenden Nullen
               10 'XX'-D-VNR                PIC  9(015).
      *
      *        Rechtsform der Firma als numerischer Wert
               10 'XX'-D-RECHTSFORM         PIC  9(002).
      *
      *        Anrede der Firma, DVAG-Interne Anredewerte
      *        0 = Unbekannt, 1 = Herr, 2 = Frau, 4 = Ehe, 5 = Firma
               10 'XX'-D-ANREDE             PIC  9(001).
      *
      *        Vorname und Nachname der Firma
               10 'XX'-D-VORNAME            PIC  X(030).
               10 'XX'-D-NACHNAME           PIC  X(030).
      *
      *        Gruendungsdatum der Firma im Format JHJJMMTT
               10 'XX'-D-GRUEND-DAT.
                  15 'XX'-D-GRUEND-DAT-JHJJ     PIC 9(004).
                  15 'XX'-D-GRUEND-DAT-MM       PIC 9(002).
                  15 'XX'-D-GRUEND-DAT-TT       PIC 9(002).
      *
      *        Firmeninhaber wegen moeglicher Mehrfachbenennung, Freitxt
               10 'XX'-D-INHABER            PIC  X(100).
      *
      *        Die Adresse der Firma
               10 'XX'-D-STRASSE            PIC  X(040).
               10 'XX'-D-PLZ                PIC  9(009).
               10 'XX'-D-ORT                PIC  X(040).
      *
      *        Postanschrift aus abweichender Postfachanschrift (PF)
               10 'XX'-D-PF-POSTFACH-NR     PIC  9(009).
               10 'XX'-D-PF-PLZ             PIC  9(009).
               10 'XX'-D-PF-ORT             PIC  X(040).
      *
      *        Kontaktdaten der Firma, Tel- Mobil- und Faxnummer
               10 'XX'-D-TEL-NR             PIC  X(030).
               10 'XX'-D-MOBIL-NR           PIC  X(030).
               10 'XX'-D-FAX-NR             PIC  X(030).
      *
      *        Daten des Ansprechpartners(AP) in der Firma
      *        Titelwerte aus OKI
               10 'XX'-D-AP-TITEL           PIC  9(002).
      *        DVAG-Interne Anredewerte
      *        0 = Unbekannt, 1 = Herr, 2 = Frau, 4 = Ehe, 5 = Firma
               10 'XX'-D-AP-ANREDE          PIC  9(001).
               10 'XX'-D-AP-VORNAME         PIC  X(030).
               10 'XX'-D-AP-NACHNAME        PIC  X(030).
               10 'XX'-D-AP-GEB-DAT.
                  15 'XX'-D-AP-GEB-DAT-JHJJ     PIC  9(004).
                  15 'XX'-D-AP-GEB-DAT-MM       PIC  9(002).
                  15 'XX'-D-AP-GEB-DAT-TT       PIC  9(002).
               10 'XX'-D-AP-EMAIL           PIC  X(128).
               10 'XX'-D-AP-FUNKTION        PIC  X(050).
               10 FILLER                    PIC  X(325).

           05  'XX'-VORSATZ REDEFINES 'XX'-DATENSATZ.
      *
      *        Hier steht der Absender, es sollte immer DV  sein
               10 'XX'-V-ABSENDER           PIC  X(003).
      *
      *        Erstelldatum des DTAs im Format JHJJMMTT
               10 'XX'-V-DATUM.
                  15 'XX'-V-DATUM-JHJJ      PIC  9(004).
                  15 'XX'-V-DATUM-MM        PIC  9(002).
                  15 'XX'-V-DATUM-TT        PIC  9(002).
      *
      *        Der empfangender Mandant DVAG oder ALLF
               10 'XX'-V-EMPFAENGER         PIC  X(010).
      *           88 'XX'-V-EMPFAENGER-DVAG     VALUE 'DVAG'.
      *           88 'XX'-V-EMPFAENGER-ALLF     VALUE 'ALLF'.
      *
      *        Der Inhalt des DTAs, in diesem Fall "KUNDENDATEN"
               10 'XX'-V-TEXT               PIC  X(020).
      *
      *        Die ART der Lieferung, Aenderungs- oder Komplettbestand
               10 'XX'-V-LIEFERUNG          PIC  X(001).
                  88 'XX'-V-LIEFERUNG-DELTA     VALUE 'D'.
                  88 'XX'-V-LIEFERUNG-KOMPL     VALUE 'K'.

           05  'XX'-NACHSATZ REDEFINES 'XX'-DATENSATZ.
               10 'XX'-N-ANZ                PIC  9(008).
      *
      ******************************************************************

