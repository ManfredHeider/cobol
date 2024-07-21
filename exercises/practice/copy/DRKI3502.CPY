      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3502.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3502.CPv  $
      *
      *   Rev 2.3   Oct 25 2018 11:19:30   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs   
      *WHO 
      *
      *   Rev 2.2   Oct 22 2018 15:50:12   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.1   Oct 22 2018 13:10:56   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.0   Oct 05 2018 13:40:18   t13ex39
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
               05  AEND-GES                 PIC  9(002).
               05  AEND-VNR                 PIC  9(015).
               05  AEND-VERARB-KZ           PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT für Vertrag
      *                U = UPDATE für Vertrag
               05  'XX'-PERSON-LFDNR        PIC  9(002).
      *            '01' für erster Kontoinhaber
               05  AEND-NNAME               PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               05  AEND-NNAME-KZ       PIC  X(001).
      *                J = NACHNAMEÄNDERUNG
               05  AEND-VNAME               PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               05  AEND-VNAME-KZ       PIC  X(001).
      *                J = VORNAMEÄNDERUNG
               05  AEND-GEBDAT              PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               05  AEND-GEBDAT-KZ      PIC  X(001).
      *                J = GEBURTSTAG ÄNDERUNG
               05  AEND-GESCHLECHT          PIC  X(001).
               05  AEND-GESCHLECHT-KZ  PIC  X(001).
      *                J = ÄNDERUNG
               05  AEND-ANREDE              PIC  9(001).
      *                ANREDEKENNZEICHEN
               05  AEND-ANREDE-KZ      PIC  X(001).
      *                J = ANREDEÄNDERUNG
               05  AEND-STR-HAUSNR     PIC  X(040).
      *                STRASSE/POSTFACH
               05  AEND-STR-HAUSNR-KZ  PIC  X(001).
      *                J = STRASSENNAMEÄNDERUNG
               05  AEND-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
               05  AEND-PLZ-KZ         PIC  X(001).
      *                J = PLZ ÄNDERUNG
               05  AEND-ORT                 PIC  X(028).
      *                ORT
               05  AEND-ORT-KZ         PIC  X(001).
      *                J = ORT ÄNDERUNG
               05  AEND-AUSL-KZ             PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               05  AEND-AUSL-KZ-KZ     PIC  X(001).
      *                J = AUSLANDSKENN. ÄNDERUNG
               05  AEND-NNAME-ALT           PIC  X(035).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
               05  AEND-VNAME-ALT           PIC  X(035).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
               05  AEND-LETZT-AENDDAT       PIC  9(008).
      *            Buchungsdatum aus Vorsatz
               05  AEND-RESERVE             PIC  X(042).



