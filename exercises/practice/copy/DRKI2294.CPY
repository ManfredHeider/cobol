      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2294.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2294.CPv  $
      *
      *   Rev 2.6   Aug 30 2011 12:49:20   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.5   Jul 07 2011 08:32:36   t13ex15
      *FKI01344: Santander Fehlerbereinigung PGM PMKI0979
      *
      *   Rev 2.4   Feb 10 2011 09:45:10   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.3   Aug 18 2010 07:28:22   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.2   Aug 13 2010 08:51:50   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.1   Aug 13 2010 08:30:04   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.0   Mar 06 2009 11:41:32   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS KUNDENDATEN VON SANTANDER,           *
      *          weitere Kontoinhaber                                  *
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00948!14.06.10! Neuerstellung                    !SWZ        *
      *--------!-------------------------------------------!-----------*
010711*FKI01344!01.07.11! Redefine VNR                     !SWZ        *
      *--------!-------------------------------------------!-----------*
110811*AKI01072!11.08.11! SEB auf Santander                !SWZ        *
      *--------!-------------------------------------------!-----------*
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
110811*01  DRKI2294.
110811 01  'XX'-SATZ.
           05  KSDS-SA                   PIC  X(002).
      *            SATZART        = '03'
           05  KSDS-GES                  PIC  9(002).
      *            GESELLSCHAFT   = '66'
           05  KSDS-KEY.
010711         10  KSDS-VNR              PIC  9(015).
010711*        10  KSDS-VNR.
010711         10  FILLER REDEFINES KSDS-VNR.
                   15 FILLER             PIC  9(004).
                   15 FILIALNUMMER       PIC  9(004).
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
      *                ANREDE...........*9 : KEINE ANGABE
            05  KSDS-STR-HAUSNR          PIC  X(028).
      *                STRASSE + Hausnummer
            05  KSDS-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
            05  KSDS-ORT                 PIC  X(028).
      *                ORT
            05  KSDS-AUSL-KZ             PIC  X(002).
      *                AUSLANDSKENNZEICHEN
            05  KSDS-DEL-KZ              PIC  X(001).
      *                DEL-KENNZEICHEN


            05  KSDS-RESERVE             PIC  X(131).
      *            RESERVE




