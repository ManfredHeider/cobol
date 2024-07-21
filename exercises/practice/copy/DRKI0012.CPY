      * T13AE38   Stand: 11/02/99    12:02:58
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0012.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0012.CPv  $
      *
      *   Rev 2.5   Oct 14 2013 09:38:44   t13ex27
      *AKI01204: SEPA - DB-Online-AML IBAN/BIC-Umstellung
      *
      *   Rev 2.4   Sep 06 2013 10:18:20   t13ex27
      *AKI01204: SEPA - DB-Online-AML-Neue Ermittlung
      *
      *   Rev 2.3   Mar 17 2013 17:21:30   t13ex27
      *AKI01204: SEPA - DB-Online - AML-Programme
      *
      *   Rev 2.2   Oct 21 1999 14:56:44   T13AE38
      *
      *
      *   Rev 2.1   Mar 25 1999 09:19:08   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.1   14 May 1998 10:33:12   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:40:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKI0007              *
      *        - EINGABE DTA KUNDENDATEN AML -                         *
      *                                                                *
      *    SATZLÄNGE 206                                              *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *29.07.97!NEUERSTELLUNG                           !A.CECCONI! 1.0*
      *KI-NEU  !                                        !         !    *
      *03.10.98!Änderung Vorsatz-text in Text Änderungs-!S.Jost   ! 1.1*
      *KI-NEU  !BESTAND UND TEXT KOMPLETTBESTAND        !         !    *
      *        !                                        !         !    *
      *AKI00053!ANPASSUNGEN WG. UMSTELLUNG              !S.JOST   ! 1.2*
      *12.10.99!BEI AML AUF PARIS(PARTNER-              !         !    *
      *        !INFORMATIONS-SYSTEM)                    !         !    *
      *        !DADURCH ÄNDERUNGEN BEI DTA-             !         !    *
      *        !SATZAUFBAU
      *         !03.01.13!2.3 ! SEPA Bankverbindung(alt)  !I. Frenzel*
      *         !        !    ! eingefügt                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRKI0012.

      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-SATZ.
      *
               20  AML-SA              PIC X(01).
      *            SATZART        = 'C'
               20  AML-VNR             PIC X(10).
      *            VERTRAGSNR.
               20  AML-ROLLE           PIC X(04).
      *            KENNZEICHEN ROLLE
121099***      20  AML-NNAME           PIC X(25).
121099         20  AML-NNAME           PIC X(40).
      *            NACHNAME
121099***      20  AML-VNAME           PIC X(20).
121099         20  AML-VNAME           PIC X(25).
      *            VORNAME DES KONTOINHABERS
      *        20  AML-GEBDAT          PIC 9(10).
               20  AML-GEBDAT          PIC 9(08).
      *            GEBURTSTAGSDATUM    JJJJMMTT
               20  AML-ANREDE          PIC X(01).
      *            ANREDESCHLÜSSEL
               20  AML-GESCHL          PIC X(01).
      *            SCHLÜSSEL GESCHLECHT
121099***      20  AML-STR-1           PIC X(34).
121099         20  AML-STR-1           PIC X(40).
      *            STRASSE
121099***      20  AML-STR-2           PIC X(10).
121099         20  AML-STR-2           PIC X(06).
      *            POSTFACH
               20  AML-AUSL-KZ         PIC X(03).
      *            AUSLANDSKENNZEICHEN
121099***      20  AML-PLZ             PIC X(06).
121099         20  AML-PLZ             PIC X(05).
      *            POSTLEITZAHL
121099***      20  AML-ORT             PIC X(30).
121099         20  AML-ORT             PIC X(34).
      *            ORT
               20  AML-BERUF           PIC X(06).
      *            BERUFSKENNZEICHEN (NICHT BERUFSGRUPPE !!)
121099***      20  AML-KOMM-ART        PIC X(02).
      *            TYP DER KOMMUNIKATIONSEINRICHTUNG (04 = FAX)
121099***      20  AML-TEL-FAX         PIC X(20).
121099         20  AML-TEL-FAX         PIC X(21).
      *            TELEFON-NR.
030113*        20  AML-BLZ             PIC 9(08).
030113*            BANKLEITZAHL
030113*        20  AML-KTO-NR          PIC 9(10).
030113*            KONTONUMMER
110913         20  AML-IBAN            PIC X(34).
110913*            INTERNATIONAL BANK ACCOUNT NUMBER
110913         20  AML-BIC             PIC X(11).
110913*            BUSINESS IDENTIFIER CODE
               20  AML-AKTUAL-KZ       PIC X(01).
      *            ÄNDERUNGSKENNZEICHEN
110913         20  FILLER              PIC X(48).
      *            Reserve
               20  DVAG-029-001-002    PIC X(001).
      *            Kennzeichen für Satzerzeugung aus Rolle 29

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  AML-VORSATZ      REDEFINES      AML-SATZ.
      *
               20  AML-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  AML-V-ABSENDER      PIC X(003).
      *            ABSENDER = 'AML'
               20  AML-V-DATUM         PIC 9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  AML-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
031098****     20  AML-V-TEXT          PIC X(015).
031098         20  AML-V-TEXT          PIC X(020).
      *            ÄNDERUNGSBESTAND = 'KUNDEN PERSONEN'
      *            KOMPLETTBESTAND  = 'KUNDEN PERSONEN INIT'
      *
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AML-NACHSATZ    REDEFINES     AML-SATZ.
      *
               20  AML-N-SA            PIC X(001).
      *            SATZART             'E'
               20  AML-N-ANZ           PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
