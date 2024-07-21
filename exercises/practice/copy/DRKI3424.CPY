      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3424.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3424.CPv  $
      *
      *   Rev 2.3   Oct 25 2018 11:19:16   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs   
      *WHO 
      *
      *   Rev 2.2   Oct 22 2018 15:50:00   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.1   Oct 22 2018 13:10:46   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.0   Sep 06 2018 11:19:28   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KUNDEN-/VERTRAGSDATEN VON Generali Invest *
      *                  für AUFBEREITUNG
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   340                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              ! WHO      *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
               05  'XX'-GES                 PIC  9(002).
               05  'XX'-VNR                 PIC  9(015).
               05  'XX'-VERARB-KZ           PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT für Vertrag
      *                U = UPDATE für Vertrag
               05  'XX'-VBNR                PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  'XX'-VBNR-KZ        PIC  X(001).
      *            J = ABSCHLUSSVERMITTLERÄNDERUNG
               05  'XX'-VBEGINN             PIC  9(008).
      *            DATUM VERTRAGSBEGINN
               05  'XX'-VBEGINN-KZ     PIC  X(001).
      *            J = DATUM VERTRAGSBEGINN ÄNDERUNG
               05  'XX'-INFOSPERRE-KZ       PIC  X(001).
      *            Kennz. für Infosperre
      *            S = INFOSPERRE GESETZT
               05  'XX'-INF-KZ-KZ      PIC  X(001).
      *            J = Kennz. geändert
               05  'XX'-INFOSPERRE-DATUM    PIC  9(008).
      *            DATUM der letzen Änderung für Datenschutzsperre
               05  'XX'-INF-DATUM-KZ   PIC  X(001).
      *            J = Datum geändert
               05  'XX'-NACHLASS-KZ         PIC  X(001).
      *            KENNZEICHEN NACHLAS
      *              J = Nachlass
               05  'XX'-NACHLASS-KZ-KZ PIC  X(001).
      *            J = Nachlass-KZ. ÄNDERUNG
               05  'XX'-PERSON-VERARB-KZ    PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT für Person
      *                U = UPDATE für Person
      *        05  'XX'-PERSON-LFDNR        PIC  9(002).
      *            '01' für erster Kontoinhaber
               05  'XX'-NNAME               PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               05  'XX'-NNAME-KZ       PIC  X(001).
      *                J = NACHNAMEÄNDERUNG
               05  'XX'-VNAME               PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               05  'XX'-VNAME-KZ       PIC  X(001).
      *                J = VORNAMEÄNDERUNG
               05  'XX'-GEBDAT              PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               05  'XX'-GEBDAT-KZ      PIC  X(001).
      *                J = GEBURTSTAG ÄNDERUNG
               05  'XX'-GESCHLECHT          PIC  X(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                1   MAENNLICH
      *                2   WEIBLICH
               05  'XX'-GESCHLECHT-KZ  PIC  X(001).
      *                J = ÄNDERUNG
               05  'XX'-ANREDE              PIC  9(001).
      *                ANREDEKENNZEICHEN
               05  'XX'-ANREDE-KZ      PIC  X(001).
      *                J = ANREDEÄNDERUNG
               05  'XX'-STR-HAUSNR     PIC  X(040).
      *                STRASSE/POSTFACH
               05  'XX'-STR-HAUSNR-KZ  PIC  X(001).
      *                J = STRASSENNAMEÄNDERUNG
               05  'XX'-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
               05  'XX'-PLZ-KZ         PIC  X(001).
      *                J = PLZ ÄNDERUNG
               05  'XX'-ORT                 PIC  X(028).
      *                ORT
               05  'XX'-ORT-KZ         PIC  X(001).
      *                J = ORT ÄNDERUNG
               05  'XX'-AUSL-KZ             PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               05  'XX'-AUSL-KZ-KZ     PIC  X(001).
      *                J = AUSLANDSKENN. ÄNDERUNG
               05  'XX'-NNAME-ALT           PIC  X(035).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
               05  'XX'-VNAME-ALT           PIC  X(035).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND

               05  'XX'-DEPOT-STATUS        PIC  X(001).
      *              Kennz. = 1, Kunde hat aktives Depot
      *              Kennz. = 0, Kunde hat kein Depot
               05  'XX'-DEPOT-STATUS-KZ     PIC  X(001).
      *                J = Depot-aktiv-Kennz. geändert
               05  'XX'-LETZT-AENDDAT       PIC  9(008).
      *            Buchungsdatum aus Vorsatz
               05  'XX'-RESERVE             PIC  X(042).





