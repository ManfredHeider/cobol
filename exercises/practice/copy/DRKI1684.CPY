      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1684.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1684.CPv  $
      *
      *   Rev 2.2   Mar 16 2005 11:46:04   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:34:06   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:37:38   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS KUNDENDATEN VON DB24,                *
      *          weitere Kontoinhaber Historie                         *
      *                                                                *
      *    SATZLÄNGE   302                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00082!11.06.04! Neuerstellung                    !WHO        *
      *        !        !                                  !           *
      *--------!-------------------------------------------!-----------*
      *FKI00992!10.03.05! Tagesdatum der Löschung          !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
       01  DRKI1684.

           05  KSDS-SA                   PIC  X(001).
      *            SATZART        = '2'
           05  KSDS-GES                  PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 FILIALNUMMER       PIC  9(003).
                   15 KUNDENNUMMER       PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               10  KSDS-PERSON-LFDNR     PIC  9(002).
      *            '01' für erster Kontoinhaber
               10 KSDS-LFDNR             PIC  9(002).
      *            LFD. NR. FÜR PRIMARY-KEY
           05  KSDS-NNAME                PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
           05  KSDS-VNAME                PIC  X(035).
      *                VORNAME DES KONTOINHABERS
           05  KSDS-GEBDAT               PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
           05  KSDS-GESCHLECHT           PIC  X(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                1   MAENNLICH
      *                2   WEIBLICH
           05  KSDS-ANREDE               PIC  X(001).
      *                ANREDE...........*1 : HERRN
      *                ANREDE...........*2 : FRAU
      *                ANREDE...........*3 : FRAEULEIN ??
      *                ANREDE...........*4 : HERRN und FRAU ??
      *                ANREDE...........*5 : HERREN ??
      *                ANREDE...........*6:  AN ??
      *                ANREDE...........*0 : KEINE ANREDE

            05  KSDS-STR                 PIC  X(028).
      *                STRASSE
            05  KSDS-HAUSNR              PIC  X(009).
      *                Haus-Nummer
            05  KSDS-POSTFACH            PIC  X(009).
      *                POSTFACH
            05  KSDS-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
            05  KSDS-ORT                 PIC  X(028).
      *                ORT
            05  KSDS-AUSL-KZ             PIC  X(003).
      *                AUSLANDSKENNZEICHEN
            05  KSDS-VORWAHL-TEL         PIC  9(006).
      *                Vorwahl zur TELEFON-NR.
            05  KSDS-TEL                 PIC  9(009).

            05  KSDS-DEPOT-AKTIV         PIC  9(001).
...   *              Kennz. = 1, Kunde hat aktives Depot
...   *              Kennz. = 0, Kunde hat kein aktives Depot
      *              Kennz. = 9, Kennz. ist noch nicht geliefert worden
100305      05  KSDS-AEND-DAT            PIC  X(10).
100305*         ÄNDERUNGSDATUM
100305      05  KSDS-RESERVE             PIC  X(093).
100305*     05  KSDS-RESERVE             PIC  X(103).

      *     05  KSDS-RESERVE             PIC  X(104).
      *            RESERVE


