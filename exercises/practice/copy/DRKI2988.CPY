      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2988.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2988.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:12:02   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:47:58   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Feb 25 2015 12:35:48   t13ex73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 9 8 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER G90 DVM Aktualisierung fuer DVMFIRMA         *
      *                                                                *
      *                                                                *
      *    SATZLAENGE  550 Bytes                                       *
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
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
           05 'XX'-GES                          PIC  9(002).
      *
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                          PIC  9(015).
      *
      *    Rechtsform der KMU
           05 'XX'-RECHTSFORM                   PIC S9(004).
           05 'XX'-RECHTSFORM-KZ                PIC  X(001).
      *    Firmeninhaber
           05 'XX'-INHABER                      PIC  X(100).
           05 'XX'-INHABER-KZ                   PIC  X(001).
      *    Postfachnummer
           05 'XX'-PF-NR                        PIC  X(010).
           05 'XX'-PF-NR-KZ                     PIC  X(001).
      *    Postfach PLZ
           05 'XX'-PF-PLZ                       PIC  9(008).
           05 'XX'-PF-PLZ-KZ                    PIC  X(001).
      *    Postfach Ort
           05 'XX'-PF-ORT                       PIC  X(040).
           05 'XX'-PF-ORT-KZ                    PIC  X(001).
      *    Telefonnummer der KMU
           05 'XX'-TEL-NR                       PIC  X(030).
           05 'XX'-TEL-NR-KZ                    PIC  X(001).
      *    Mobilnnummer der KMU
           05 'XX'-MOBIL-NR                     PIC  X(030).
           05 'XX'-MOBIL-NR-KZ                  PIC  X(001).
      *    Faxnummer der KMU
           05 'XX'-FAX-NR                       PIC  X(030).
           05 'XX'-FAX-NR-KZ                    PIC  X(001).
      *    Titel des Ansprechpartners der KMU
           05 'XX'-AP-TITEL                     PIC S9(004).
           05 'XX'-AP-TITEL-KZ                  PIC  X(001).
      *    Anrede des Ansprechpartners der KMU
           05 'XX'-AP-ANREDE                    PIC S9(004).
           05 'XX'-AP-ANREDE-KZ                 PIC  X(001).
      *    Vorname des Ansprechpartners der KMU
           05 'XX'-AP-VORNAME                   PIC  X(030).
           05 'XX'-AP-VORNAME-KZ                PIC  X(001).
      *    Nachname des Ansprechpartners der KMU
           05 'XX'-AP-NACHNAME                  PIC  X(030).
           05 'XX'-AP-NACHNAME-KZ               PIC  X(001).
      *    Geb-Dat des Ansprechpartners der KMU
           05 'XX'-AP-GEBDAT                    PIC  X(010).
           05 'XX'-AP-GEBDAT-KZ                 PIC  X(001).
      *    EMail des Ansprechpartners der KMU
           05 'XX'-AP-EMAIL                     PIC  X(128).
           05 'XX'-AP-EMAIL-KZ                  PIC  X(001).
      *    Funktion des Ansprechpartners der KMU
           05 'XX'-AP-FUNKTION                  PIC  X(050).
           05 'XX'-AP-FUNKTION-KZ               PIC  X(001).
      *
           05 FILLER                            PIC  X(009).
      ******************************************************************

