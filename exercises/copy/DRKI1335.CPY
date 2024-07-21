      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1335.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI1335.CPv  $
      *
      *   Rev 2.12   Nov 24 2020 09:17:00   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.11   Jun 25 2019 09:13:52   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.10   Jun 04 2019 14:45:00   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.9   Mar 18 2019 07:30:08   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.8   Mar 17 2019 15:20:26   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.7   Mar 17 2019 15:11:38   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.6   Sep 02 2013 12:14:50   t13ex14
      *AKI01204: SEPA - DB-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.5   Sep 02 2013 09:57:30   t13ex14
      *AKI01204: SEPA - DB-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.4   Jul 05 2004 10:33:28   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.3   Jul 13 2003 23:45:20   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.2   Feb 25 2003 15:44:36   t13ex12
      *AKI00328 DB-Kunden ohne Anlagedepot
      *
      *   Rev 2.1   Jul 30 2002 16:48:32   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Dec 14 2001 09:20:56   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 1 3 3 5  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND             *
      *          KUNDENDATENäNDERUNGEN von DB24                        *
      *                                                                *
      *    LAENGE =  450 BYTES      ERSTELLT  AM 21.03.02              *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 650                          *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!21.03.02! Neuerstellung                    !v.d.Goltz  *
      *----------------------------------------------------------------*
      *AKI00328!08.01.03! Neues Kennz. für Kunden ohne     !v.d.Goltz  *
      *        !        ! Anlagedepot. Namen DB24 in DB.   !           *
      * ---------------------------------------------------------------*
      *AKI00327!28.05.03! BLZ verarbeiten.                 !v.d.Goltz  *
      *----------------------------------------------------------------*
      *AKI00082!01106.04! Neues Kennz. bei physischer Ver- !WHO        *
      *        !        ! tragslöschung Bei DEUTSCHER BANK !           *
      *----------------------------------------------------------------*
      *AKI01204!24.07.13! NEUE FELDER EINGEFÜGT f. SEPA    !WMI        *
      *        !        !   UND DEN FILLER ANGEPASST       !           *
      *----------------------------------------------------------------*
      *AKI01625!24.02.19! DEPOT-KZ entfernt                !WHO        *
      *        !        ! DEPOT-AKTIV entfern              !           *
      * -------+--------+----+-----------------------------+-----------*
      *AKI01710!17.04.19! neue Felder Filialanschrift      ! WHO       *
      *----------------------------------------------------------------*
      * AKI01805!14.10.20! NEUES ATTRIBUT ESAFE           ! ATAR       *
      *                                                                *
      *        !        !                                  !           *
      ******************************************************************
      *
       01  DRKI1335.
               05  BNK-SA                  PIC  X(001).
      *            SATZART
               05  BNK-GES                 PIC  9(002).
      *            GESELLSCHAFT
               05  BNK-VNR.
                   10 FILIALNUMMER         PIC  9(003).
                   10 KUNDENNUMMER         PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  BNK-VERARB-KZ           PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT für Vertrag
      *                U = UPDATE für Vertrag
               05  BNK-VBNR                PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  BNK-AEND-VBNR-KZ        PIC  X(001).
      *            J = ABSCHLUSSVERMITTLERÄNDERUNG
               05  BNK-VBEGINN             PIC  9(008).
      *            DATUM VERTRAGSBEGINN
               05  BNK-AEND-VBEGINN-KZ     PIC  X(001).
      *            J = DATUM VERTRAGSBEGINN ÄNDERUNG
               05  BNK-VABLAUF             PIC  9(008).
      *            DATUM VERTRAGSABLAUF
               05  BNK-AEND-VABLAUF-KZ     PIC  X(001).
      *            J = DATUM VERTRAGSABLAUF ÄNDERUNG
               05  BNK-INFOSPERRE-KZ       PIC  X(001).
      *            Kennz. für Infosperre
      *            S = INFOSPERRE GESETZT
               05  BNK-AEND-INF-KZ-KZ      PIC  X(001).
      *            J = Kennz. geändert
               05  BNK-INFOSPERRE-DATUM    PIC  9(008).
      *            DATUM der letzen Änderung für Datenschutzsperre
               05  BNK-AEND-INF-DATUM-KZ   PIC  X(001).
      *            J = Datum geändert
               05  BNK-VERFUEG-KZ          PIC  X(001).
      *            KENNZEICHEN VERFÜGUNG
               05  BNK-AEND-VERFUEG-KZ     PIC  X(001).
      *            J = VERFÜGUNGSKENNZ. ÄNDERUNG
               05  BNK-NACHLASS-KZ         PIC  X(001).
      *            KENNZEICHEN NACHLAS
      *              J = Nachlass
               05  BNK-AEND-NACHLASS-KZ    PIC  X(001).
      *            J = Nachlass-KZ. ÄNDERUNG
               05  BNK-TELREVERS           PIC  X(001).
      *            Telefonrevers = 'J'
               05  BNK-AEND-TELREVERS-KZ   PIC  X(001).
      *            J = Telefon-Revers ÄNDERUNG
               05  BNK-ONLREVERS           PIC  X(001).
      *            Onlinerevers = 'J'
               05  BNK-AEND-ONLREVERS-KZ   PIC  X(001).
      *            J = Online-Revers ÄNDERUNG

               05  BNK-PERSON-VERARB-KZ    PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT für Person
      *                U = UPDATE für Person
               05  BNK-PERSON-LFDNR        PIC  9(002).
      *            '01' für erster Kontoinhaber
               05  BNK-NNAME               PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               05  BNK-AEND-NNAME-KZ       PIC  X(001).
      *                J = NACHNAMEÄNDERUNG
               05  BNK-VNAME               PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               05  BNK-AEND-VNAME-KZ       PIC  X(001).
      *                J = VORNAMEÄNDERUNG
               05  BNK-GEBDAT              PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               05  BNK-AEND-GEBDAT-KZ      PIC  X(001).
      *                J = GEBURTSTAG ÄNDERUNG
               05  BNK-GESCHLECHT          PIC  X(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                1   MAENNLICH
      *                2   WEIBLICH
               05  BNK-AEND-GESCHLECHT-KZ  PIC  X(001).
      *                J = ÄNDERUNG
               05  BNK-ANREDE              PIC  9(001).
      *                ANREDEKENNZEICHEN
               05  BNK-AEND-ANREDE-KZ      PIC  X(001).
      *                J = ANREDEÄNDERUNG
               05  BNK-STR                 PIC  X(028).
      *                STRASSE/POSTFACH
               05  BNK-AEND-STR-KZ         PIC  X(001).
      *                J = STRASSENNAMEÄNDERUNG
               05  BNK-HAUSNR              PIC  X(009).
      *                Hausnr der STRASSE
               05  BNK-AEND-HAUSNR-KZ      PIC  X(001).
      *                J = ÄNDERUNG Hausnr.
               05  BNK-POSTFACH            PIC  X(009).
      *                POSTFACH
               05  BNK-AEND-PF-KZ          PIC  X(001).
      *                J = ÄNDERUNG Postfach
               05  BNK-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
               05  BNK-AEND-PLZ-KZ         PIC  X(001).
      *                J = PLZ ÄNDERUNG
               05  BNK-ORT                 PIC  X(028).
      *                ORT
               05  BNK-AEND-ORT-KZ         PIC  X(001).
      *                J = ORT ÄNDERUNG
               05  BNK-AUSL-KZ             PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               05  BNK-AEND-AUSL-KZ        PIC  X(001).
      *                J = AUSLANDSKENN. ÄNDERUNG
               05  BNK-VORWAHL-TEL         PIC  9(006).
      *                Vorwahl zur TELEFON-NR.
               05  BNK-AEND-VORWAHL-KZ     PIC  X(001).
      *                J = Vorwahl Telefonnr. ÄNDERUNG
               05  BNK-TEL                 PIC  9(009).
      *                TELEFON-NR.
               05  BNK-AEND-TEL-KZ         PIC  X(001).
      *                J = TELEFONNR ÄNDERUNG
               05  BNK-VOR-TEL-ALT         PIC  9(006).
      *                VORHERIGE Vorwahl TELEFONNR DES KONTOINHABERS
               05  BNK-TEL-ALT             PIC  9(009).
      *                VORHERIGE TELEFONNR DES KONTOINHABERS
               05  BNK-NNAME-ALT           PIC  X(035).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
               05  BNK-VNAME-ALT           PIC  X(035).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND

               05  BNK-WKZ                 PIC  X(003).
               05  BNK-LETZT-AENDDAT       PIC  9(008).
      *            Buchungsdatum aus Vorsatz

               05  FILLER                  PIC  X(002).

               05  BNK-BLZ                 PIC  9(008).
...   *              BLZ des Kundenkontos
...            05  BNK-AEND-BLZ-KZ         PIC  X(001).
      *                J = BLZ ÄNDERUNG

               05  BNK-DEL-KZ             PIC  X(001).
      *              DEL-Kennzeichen
      *
               05  BNK-BIC                 PIC  X(011).
      *              BIC des Belastungskontos zur Lastschrift
 
               05  BNK-AEND-BIC-KZ         PIC  X(001).
      *                J = BLZ ÄNDERUNG
               05  BNK-KND-NAME            PIC  X(028).
      *            Kundenname
               05  BNK-KND-NAME-KZ         PIC  X(001).

               05  BNK-FILIALE-ART         PIC  X(035).
      *            Art der Filiale
               05  BNK-FILIALE-ART-KZ      PIC  X(001).

               05  BNK-FILIALE-NAME        PIC  X(035).
      *            Name der Filiale
               05  BNK-FILIALE-NAME-KZ     PIC  X(001).

               05  BNK-FILIALE-STR         PIC  X(035).
      *            Adresse (Straße und Hausnr.)        .
               05  BNK-FILIALE-STR-KZ      PIC  X(001).

               05  BNK-FILIALE-ORT         PIC  X(035).
      *            Adresse (PLZ und Ort)
               05  BNK-FILIALE-ORT-KZ      PIC  X(001).

               05  BNK-ESAFE               PIC  X(001).
      *            ESAFE = 'J'
               05  BNK-AEND-ESAFE-KZ       PIC  X(001).
      *            J = ESAFE ÄNDERUNG

               05  BNK-RESERVE             PIC  X(099).



