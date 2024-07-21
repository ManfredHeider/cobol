      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1351.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/cpy/DRKI1351.CPv  $
      *
      *   Rev 2.3   Jul 13 2003 23:46:38   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.2   Feb 25 2003 15:47:50   t13ex12
      *AKI00328 DB-Kunden ohne Anlagedepot
      *
      *   Rev 2.1   Jul 30 2002 16:48:42   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Mar 12 2002 12:34:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS KUNDENDATEN VON DB24,                *
      *          weitere Kontoinhaber                                  *
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!29.04.02! Neuerstellung                    !v.d.Goltz  *
      *----------------------------------------------------------------*
      *AKI00nnn!08.01.03! Neues Kennz. für Kunden ohne     !v.d.Goltz  *
      *        !        ! Depot.                           !           *
      * ---------------------------------------------------------------*
      *AKI00327!28.05.03! Auftragsnr. und Datum in Änd.-   !v.d.Goltz  *
      *        !        ! Zeilen haben bei letzter Prod.-  !           *
      *        !        ! Übergabe gefehlt.                !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
       01  DRKI1351.

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


            05  KSDS-RESERVE             PIC  X(103).
      *     05  KSDS-RESERVE             PIC  X(104).
      *            RESERVE



