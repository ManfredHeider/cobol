      *k--------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3381.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3381.CPv  $
      *
      *   Rev 2.5   Nov 05 2018 15:56:44   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs   
      *WHO 
      *
      *   Rev 2.4   Nov 05 2018 15:17:26   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.3   Oct 25 2018 11:19:06   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.2   Oct 22 2018 15:49:44   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.1   Oct 22 2018 13:10:36   t13ex39
      *AKI01615: Generali Invest Generali Invest, COPYs und PGMs
      *WHO
      *
      *   Rev 2.0   Aug 21 2018 16:22:56   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für zusätzliche Kontoinhaber von Generali     *
      *      Invest                                                    *
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
       01  DRKI3381.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(001).
      *            SATZART = 'A'
                   88 G42-SATZART-VORSATZ VALUE 'A'.
               15  'XX'-V-ABSENDER      PIC  X(027).
      *            ABSENDER = 'GEI'
               15  'XX'-V-DATUM         PIC  9(008).
      *            Dadtum = 'JJJESTTT' Erstellung
               15  'XX'-V-DATUM-STAND   PIC  9(008).
      *            Dadtum = 'JJJJMMTT' Stand
               15  'XX'-V-EMPFAENGER    PIC  X(027).
      *            EMPFAENGER = 'ALAG'
                   88 G42-V-EMPFAENGER-ALAG   VALUE 'ALAG'.
               15  'XX'-V-TEXT          PIC  X(020).
      *            INHALTSANGABE:'KUNDENSTAMM-WEITERE'
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
               15  'XX'-VNR                PIC  9(015) .
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               15  'XX'-PERSON-LFDNR       PIC  9(002).
      *            ab '02' für zusätzliche Kontoinhaber
               15  'XX'-NNAME              PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               15  'XX'-VNAME              PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               15  'XX'-GEBDAT             PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               15  'XX'-GESCHLECHT         PIC  X(001).
               15  'XX'-ANREDE             PIC  X(001).
      *                ANREDE...........*3 : HERRN
      *                ANREDE...........*1 : FRAU
      *                ANREDE...........*4 : FIRMA
      *                ANREDE...........*5 : SONSTIGES
               15  'XX'-STR-HAUSNR         PIC  X(040).
      *                STRASSE + STR
               15  'XX'-PLZ                PIC  X(009).
      *                POSTLEITZAHL
               15  'XX'-ORT                PIC  X(040).
      *                ORT
               15  'XX'-AUSL-KZ            PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               15  'XX'-RESERVE            PIC  X(058).
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
                   88 G42-SATZART-ENDESATZ        VALUE 'E'.
               15  'XX'-N-ANZ              PIC  9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                  PIC  X(245).
      *            RESERVE










