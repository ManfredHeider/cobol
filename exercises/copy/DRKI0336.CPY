      * T13AE38   Stand: 23/12/98    17:25:58
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0336.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0336.CPv  $
      *
      *   Rev 2.5   Jan 13 2014 13:39:46   t13ex27
      *FKI01438: AML-Ermittlung-Korrekturprogramme
      *
      *   Rev 2.4   Oct 14 2013 09:39:00   t13ex27
      *AKI01204: SEPA - DB-Online-AML IBAN/BIC-Umstellung
      *
      *   Rev 2.3   Sep 06 2013 10:18:38   t13ex27
      *AKI01204: SEPA - DB-Online-AML-Neue Ermittlung
      *
      *   Rev 2.2   Oct 21 1999 14:56:54   T13AE38
      *
      *
      *   Rev 2.1   Mar 25 1999 09:19:28   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:00:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG AML                                                     *
      *     - AUSGABEDATEI ALLG. KUNDENDATEN (ALLE GESELLSCHAFTEN) -   *
      *                                                                *
      *    SATZLÄNGE   321                                             *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *06.08.97!NEUERSTELLUNG                          !S.JOST   ! 01  *
      *AKI00001!                                       !         !     *
      *AKI00053!ANPASSUNGEN WG. UMSTELLUNG             !S.JOST   ! 1.1 *
      *12.10.99!BEI AML AUF PARIS(PARTNER-             !         !     *
      *        !INFORMATIONS-SYSTEM)                   !         !     *
      *        !DADURCH ÄNDERUNGEN BEI DTA-            !         !     *
      *        !SATZAUFBAU                                             *
      *AKI01204!SEPA-Umstellung-Neue Ermittlung        !I.Frenzel! 2.3 *
      *AKI01204!SEPA-Umstellung(BIC, IBAN)             !I.Frenzel! 2.4 *
      *FKI01438!Änderung für REPLACING                 !I.Frenzel! 2.5 *

      ******************************************************************
      *
      *01  AML-PERSONEN-SATZ.
           05  'P'-KEY.
               10  'P'-GES                     PIC 9(02).
               10  'P'-VNR                     PIC 9(15).
      *
           05  'P'-PERSONENDATEN.
               10  'P'-ROLLE-VERTRAG           PIC X(01).
               10  'P'-ROLLE-TARIF             PIC X(01).
               10  'P'-VTRBEZ-ARTKZ            PIC X(01).

               10  'P'-NNAME                   PIC X(40).
               10  'P'-AEND-NNAME-KZ           PIC X(01).
121099***      10  'P'-VNAME                   PIC X(20).
121099         10  'P'-VNAME                   PIC X(25).
               10  'P'-AEND-VNAME-KZ           PIC X(01).
               10  'P'-GEBDAT                  PIC 9(08).
               10  'P'-AEND-GEBDAT-KZ          PIC X(01).
               10  'P'-ANREDE                  PIC X(01).
               10  'P'-AEND-ANREDE-KZ          PIC X(01).
               10  'P'-GESCHL                  PIC X(01).
               10  'P'-AEND-GESCHL-KZ          PIC X(01).
               10  'P'-NNAME-ALT               PIC X(40).
121099***      10  'P'-VNAME-ALT               PIC X(20).
121099         10  'P'-VNAME-ALT               PIC X(25).
               10  'P'-GEBDAT-ALT              PIC 9(08).
121099***      10  'P'-STR-1                   PIC X(34).
121099         10  'P'-STR-1                   PIC X(40).
               10  'P'-AEND-STR-1-KZ           PIC X(01).
121099***      10  'P'-STR-2                   PIC X(10).
121099         10  'P'-STR-2                   PIC X(06).
               10  'P'-AEND-STR-2-KZ           PIC X(01).
121099***      10  'P'-PLZ                     PIC X(06).
121099         10  'P'-PLZ                     PIC X(05).
               10  'P'-AEND-PLZ-KZ             PIC X(01).
121099***      10  'P'-ORT                     PIC X(30).
121099         10  'P'-ORT                     PIC X(34).
               10  'P'-AEND-ORT-KZ             PIC X(01).
               10  'P'-AUSL-KZ                 PIC X(03).
               10  'P'-AEND-AUSL-KZ            PIC X(01).
           05  'P'-BEITRAGSZAHLERDATEN.
110913*        10  'P'-BIC                     PIC 9(009).
>>    *        10  'P'-AEND-BIC-KZ             PIC X(001).
      *        10  'P'-KTO-NR                  PIC X(015).
      *        10  'P'-AEND-KTO-KZ             PIC X(001).
               10  'P'-ABW-BZ                  PIC X(001).
      *        10  'P'ILLER                    PIC X(027).
               10  'P'-BIC                     PIC X(11).
               10  'P'-AEND-BIC-KZ             PIC X(001).
    <<         10  'P'-IBAN                    PIC X(34).
110913         10  'P'-AEND-IBAN-KZ            PIC X(001).
      *
           05  'P'-AKTUAL-KZ                  PIC X(01).
