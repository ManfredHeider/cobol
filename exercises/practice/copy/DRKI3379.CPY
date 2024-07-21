      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3379.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3379.CPv  $
      *
      *   Rev 2.5   Nov 05 2018 15:56:32   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs   
      *WHO 
      *
      *   Rev 2.4   Nov 05 2018 15:17:08   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.3   Oct 25 2018 11:18:50   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.2   Oct 22 2018 15:49:30   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.1   Oct 22 2018 13:10:24   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.0   Aug 21 2018 16:22:42   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KUNDEN-/VERTRAGSDATEN VON Generali Invest *
      *                                                                *
      *    SATZLÄNGE   250                                             *
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
      *
CCC009*    SKIP1
       01  DRKI3379.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(001).
      *            SATZART = '1'
                   88 'XX'-SATZART-VORSATZ VALUE 'A'.
               15  'XX'-V-ABSENDER      PIC  X(027).
      *            ABSENDER = 'GEI'
               15  'XX'-V-DATUM         PIC  9(008).
      *            Dadtum = 'JJJESTTT' Erstellung
               15  'XX'-V-DATUM-STAND   PIC  9(008).
      *            Dadtum = 'JJJJMMTT' Stand
               15  'XX'-V-EMPFAENGER    PIC  X(027).
      *            EMPFAENGER = 'ALAG'
                   88 'XX'-V-EMPFAENGER-ALAG   VALUE 'ALAG'.
               15  'XX'-V-TEXT          PIC  X(020).
      *            INHALTSANGABE:'KUNDENSTAMM'
               15  'XX'-V-ART           PIC  X(001).
      *            K = Komplettbestand, D für Delta
               15  FILLER               PIC  X(158).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                 PIC  X(001).
      *            SATZART        = 'C'
               15  'XX'-GES                PIC  9(002).
      *            GESELLSCHAFT   = '42'
               15  'XX'-VNR                PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN
      *            UND UNTERRDEPOTNUMMER
               15  'XX'-VBNR               PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               15  'XX'-VBEGINN            PIC  9(008).
      *            DATUM VERTRAGSBEGINN/Kundeneröffnung
               15  'XX'-DEPOT-STATUS       PIC  9(001).
      *            1 = aktiv, 2 = geschlossen
               15  'XX'-INFOSPERRE-KZ      PIC  X(001).
      *            S = INFOSPERRE GESETZT
               15  'XX'-INFOSPERRE-DATUM   PIC  9(008).
      *            DATUM der letzen Änderung für Datenschutzsperre
               15  'XX'-NACHLASS-KZ        PIC  X(001).
      *            KENNZEICHEN NACHLAS
      *              J = Nachlass
               15  'XX'-TELREVERS          PIC  X(001).
      *            Telefonrevers = 'J'
               15  'XX'-ONLREVERS          PIC  X(001).
      *            Onlinerevers = 'J'
               15  'XX'-NNAME              PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               15  'XX'-VNAME              PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               15  'XX'-GEBDAT             PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               15  'XX'-GESCHLECHT         PIC  9(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                3   MAENNLICH
      *                1   WEIBLICH
      *                4   FIRMA
      *                5   SONSTIGES
               15  'XX'-ANREDE             PIC  9(001).
      *                ANREDE...........*3 : HERR
      *                ANREDE...........*1 : FRAU
      *                ANREDE...........*4 : FIRMA
      *                ANREDE...........*5 : SONSTIGES

               15  'XX'-STR-HAUSNR         PIC  X(040).
      *                STRASSE
               15  'XX'-PLZ                PIC  X(009).
      *                POSTLEITZAHL
               15  'XX'-ORT                PIC  X(040).
      *                ORT
               15  'XX'-AUSL-KZ            PIC  X(003).
      *                ISO-Code
               15  'XX'-RESERVE            PIC  X(032).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               15  'XX'-N-SA               PIC  X(001).
      *            SATZART             'E'
                   88 'XX'-SATZART-ENDESATZ        VALUE 'E'.
               15  'XX'-N-ANZ              PIC  9(007) COMP-3.
      *            ANZAHL SAETZE ohne VOR- UND NACHSATZ
               15  FILLER                  PIC  X(245).
      *            RESERVE







