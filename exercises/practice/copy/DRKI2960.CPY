      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2960.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2960.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 08:21:32   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:10:06   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:02   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 11:39:16   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 6 0    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DVM Aenderungsdaten Kunde                *
      *                                                                *
      *    SATZLAENGE  1022 Bytes                                      *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      AENDERUNG                        !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.15!NEUERSTELLUNG                          !DDY      ! 01  *
      *--------!---------------------------------------!---------!-----*
120218* AKI01572 !12.02.18!2.3 ! Projekt "Frischer Wind"   ! IFR       *
      *          !        !    ! Aufname NAME-ALT          !           *
      *          !        !    !                           !           *
      *          !        !    !                           !           *
      ******************************************************************
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *    Gesellschaftsid
           05 'XX'-GES                          PIC  9(002).
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                          PIC  9(015).
      *
      *    Rechtsform der Firma als numerischer Wert
           05 'XX'-RECHTSFORM                   PIC  9(002).
           05 'XX'-RECHTSFORM-KZ                PIC  X(001).
      *
      *    Anrede der Firma, DVAG-Interne Anredewerte
      *    0 = Unbekannt, 1 = Herr, 2 = Frau, 4 = Ehe, 5 = Firma
           05 'XX'-ANREDE                       PIC  9(001).
           05 'XX'-ANREDE-KZ                    PIC  X(001).
      *
      *    Vorname und Nachname der Firma
           05 'XX'-VORNAME                      PIC  X(030).
           05 'XX'-VORNAME-KZ                   PIC  X(001).
           05 'XX'-NACHNAME                     PIC  X(030).
           05 'XX'-NACHNAME-KZ                  PIC  X(001).
      *
      *    Gruendungsdatum der Firma im Format JHJJMMTT
           05 'XX'-GRUEND-DAT.
               10 'XX'-GRUEND-DAT-JHJJ          PIC 9(004).
               10 'XX'-GRUEND-DAT-MM            PIC 9(002).
               10 'XX'-GRUEND-DAT-TT            PIC 9(002).
           05 'XX'-GRUEND-DAT-KZ                PIC  X(001).
      *
120218*    Namensänderung
120218     05 'XX'-NAME-ALT.
120218         10 'XX'-NNAME-ALT                PIC X(30).
120218         10 'XX'-VNAME-ALT                PIC X(30).
120218*
      *    Firmeninhaber wegen moeglicher Mehrfachbenennung, Freitext
           05 'XX'-INHABER                      PIC  X(100).
           05 'XX'-INHABER-KZ                   PIC  X(001).
      *
      *    Die Adresse der Firma
           05 'XX'-STRASSE                      PIC  X(040).
           05 'XX'-STRASSE-KZ                   PIC  X(001).
           05 'XX'-PLZ                          PIC  9(009).
           05 'XX'-PLZ-KZ                       PIC  X(001).
           05 'XX'-ORT                          PIC  X(040).
           05 'XX'-ORT-KZ                       PIC  X(001).
      *
      *    Postanschrift aus abweichender Postfachanschrift (PF)
           05 'XX'-PF-POSTFACH-NR               PIC  9(009).
           05 'XX'-PF-POSTFACH-NR-KZ            PIC  X(001).
           05 'XX'-PF-PLZ                       PIC  9(009).
           05 'XX'-PF-PLZ-KZ                    PIC  X(001).
           05 'XX'-PF-ORT                       PIC  X(040).
           05 'XX'-PF-ORT-KZ                    PIC  X(001).
      *
      *    Kontaktdaten der Firma, Tel- Mobil- und Faxnummer
           05 'XX'-TEL-NR                       PIC  X(030).
           05 'XX'-TEL-NR-KZ                    PIC  X(001).
           05 'XX'-MOBIL-NR                     PIC  X(030).
           05 'XX'-MOBIL-NR-KZ                  PIC  X(001).
           05 'XX'-FAX-NR                       PIC  X(030).
           05 'XX'-FAX-NR-KZ                    PIC  X(001).
      *
      *    Daten des Ansprechpartners(AP) in der Firma
      *    Titelwerte aus OKI
           05 'XX'-AP-TITEL                     PIC  9(002).
           05 'XX'-AP-TITEL-KZ                  PIC  X(001).
      *    DVAG-Interne Anredewerte
      *    0 = Unbekannt, 1 = Herr, 2 = Frau, 4 = Ehe, 5 = Firma
           05 'XX'-AP-ANREDE                    PIC  9(001).
           05 'XX'-AP-ANREDE-KZ                 PIC  X(001).
           05 'XX'-AP-VORNAME                   PIC  X(030).
           05 'XX'-AP-VORNAME-KZ                PIC  X(001).
           05 'XX'-AP-NACHNAME                  PIC  X(030).
           05 'XX'-AP-NACHNAME-KZ               PIC  X(001).
           05 'XX'-AP-GEB-DAT.
               10 'XX'-AP-GEB-DAT-JHJJ          PIC  9(004).
               10 'XX'-AP-GEB-DAT-MM            PIC  9(002).
               10 'XX'-AP-GEB-DAT-TT            PIC  9(002).
           05 'XX'-AP-GEB-DAT-KZ                PIC  X(001).
           05 'XX'-AP-EMAIL                     PIC  X(128).
           05 'XX'-AP-EMAIL-KZ                  PIC  X(001).
           05 'XX'-AP-FUNKTION                  PIC  X(050).
           05 'XX'-AP-FUNKTION-KZ               PIC  X(001).
120218*    05 FILLER                            PIC  X(325).
120218     05 FILLER                            PIC  X(265).
      ******************************************************************

