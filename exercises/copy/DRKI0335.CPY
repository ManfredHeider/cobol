      * T13AE38   Stand: 30/08/98    15:34:12
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0335.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0335.CPv  $
      *
      *   Rev 2.5   Oct 14 2013 09:38:54   t13ex27
      *AKI01204: SEPA - DB-Online-AML IBAN/BIC-Umstellung
      *
      *   Rev 2.4   Sep 06 2013 10:18:32   t13ex27
      *AKI01204: SEPA - DB-Online-AML-Neue Ermittlung
      *
      *   Rev 2.3   Mar 17 2013 17:28:08   t13ex27
      *AKI01204: SEPA - DB-Online - AML-Programme
      *
      *   Rev 2.2   Oct 21 1999 14:56:50   T13AE38
      *
      *
      *   Rev 2.1   Mar 25 1999 11:43:52   T13AE38
      *
      *
      *   Rev 2.0   Mar 25 1999 11:25:26   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:00:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKI0007              *
      *        - EINGABE DTA KUNDENDATEN AML                           *
      *    KSDS-DATEI                                                  *
      *                                                                *
      *    SATZLÄNGE 206                                               *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *16.07.98!NEUERSTELLUNG                           !S.JOST   ! 01 *
      *AKI00001!                                        !         !    *
      *        !                                        !         !    *
      *AKI00053!ANPASSUNGEN WG. UMSTELLUNG              !S.JOST   ! 1.1*
      *12.10.99!BEI AML AUF PARIS(PARTNER-              !         !    *
      *        !INFORMATIONS-SYSTEM)                    !         !    *
      *        !DADURCH ÄNDERUNGEN BEI DTA-             !         !    *
      *        !SATZAUFBAU
      *         !03.01.13!2.13! SEPA Bankverbindung(alt)  !I. Frenzel*
      *         !        !    ! und IBAN/BIC eingefügt    !          *
      *AKI01204 !31.05.13!2.4 ! Beruf und Telefon entf.   !I. Frenzel*
      ****************************************************************
      *
      *
       01  AML-KOMPLETT-SATZ.
               05  KOMPL-AML-SA        PIC X(01).
      *            SATZART        = 'C'
               05  KOMPL-AML-KEY.
                   10  KOMPL-AML-VNR   PIC X(10).
      *                VERTRAGSNR.
                   10  KOMPL-AML-ROLLE PIC X(04).
      *                KENNZEICHEN ROLLE

121099***      05  KOMPL-AML-NNAME     PIC X(25).
121099         05  KOMPL-AML-NNAME     PIC X(40).
      *            NACHNAME
121099***      05  KOMPL-AML-VNAME     PIC X(20).
121099         05  KOMPL-AML-VNAME     PIC X(25).
      *            VORNAME DES KONTOINHABERS
      *        05  KOMPL-AML-GEBDAT    PIC 9(10).
               05  KOMPL-AML-GEBDAT    PIC 9(08).
      *            GEBURTSTAGSDATUM    JJJJMMTT
               05  KOMPL-AML-ANREDE    PIC X(01).
      *            ANREDESCHLÜSSEL
               05  KOMPL-AML-GESCHL    PIC X(01).
      *            SCHLÜSSEL GESCHLECHT
121099***      05  KOMPL-AML-STR-1     PIC X(34).
121099         05  KOMPL-AML-STR-1     PIC X(40).
      *            STRASSE
121099***      05  KOMPL-AML-STR-2     PIC X(10).
121099         05  KOMPL-AML-STR-2     PIC X(06).
      *            POSTFACH
               05  KOMPL-AML-AUSL-KZ   PIC X(03).
      *            AUSLANDSKENNZEICHEN
121099***      05  KOMPL-AML-PLZ       PIC X(06).
121099         05  KOMPL-AML-PLZ       PIC X(05).
      *            POSTLEITZAHL
121099***      05  KOMPL-AML-ORT       PIC X(30).
121099         05  KOMPL-AML-ORT       PIC X(34).
      *            ORT
110913*        05  KOMPL-AML-BLZ       PIC 9(08).
110913*            BANKLEITZAHL
110913*        05  KOMPL-AML-KTO-NR    PIC 9(10).
110913*            KONTONUMMER
110913*        05  KOMPL-AML-RESERVE   PIC X(27).
110913*            Reserve
110913         05  KOMPL-AML-BIC       PIC X(11).
110913*            BUSINESS IDENTIFIER CODE
110913         05  KOMPL-AML-IBAN      PIC X(34).
110913*            INTERNATIONAL BANK ACCOUNT NUMBER
               05  KOMPL-AML-AKTUAL-KZ PIC X(01).
      *            ÄNDERUNGSKENNZEICHEN
