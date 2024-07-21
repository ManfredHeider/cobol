      *---------------------------------------------------------------*
      *    Standard f黵 alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0496.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0496.CPv  $
      *
      *   Rev 2.5   Aug 22 2013 11:55:36   t13ex73
      *AKI01204: SEPA - DB-Online Badenia PGM/COPY
      *
      *   Rev 2.4   Apr 10 2012 13:53:06   t13ex52
      *FKI01366: BADENIA: Fehlender Ausgabesatz 2.VN f黵 Kundenaktu
      *alisierung
      *
      *   Rev 2.3   Oct 12 2010 17:50:34   t13ex52
      *AKI00968: Mitinhaber Badenia-Vertrag als vollwertigen VN ver
      *arbeiten
      *
      *   Rev 2.2   Oct 26 2009 07:36:26   t13ex14
      *AKI00846: Erw. der Bestands-DTAs Badenia: Programme u. Auftr
      *鋑e
      *
      *   Rev 2.1   Apr 08 1999 15:30:12   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:06:20   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   28 Jul 1998 09:58:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 4 9 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BADENIA-膎derungsdaten                       *
      *    Kundendaten BADENIA mit 膎derungskennung je Feld            *
      *                                                                *
      *    SATZL腘GE   290                                             *
120809*    SATZL腘GE   700                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      腘DERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.08.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
120809*AKI00846! DTA-Erweiterung f. Mitinhabe (MI)     !W.Misgeld!     *
      *        !                                       !         !     *
060312*FKI01366! Integration neues Kennzeichen fuer die!H.Schlenther   *
060312*   UE001! Aufbereitung der Kundenaktualisierung !         !     *
      *--------!---------------------------------------!---------!-----*
310713*AKI01204! SEPA-Einf黨rung: Erweiterung um den   !DDY      !     *
      *        ! abweichenden Beitragszahler und die   !         !     *
      *        ! Mandatsreferenznummer                 !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0496.
      *
           05  AE-AEND-KZ                 PIC  X(001).
      *
           05  AE-VERTRAGS-NR             PIC  9(015).
      *
           05  AE-NACHNAME                PIC  X(040).
           05  AE-NACHNAME-AEKZ           PIC  X(001).
      *
           05  AE-VORNAME                 PIC  X(030).
           05  AE-VORNAME-AEKZ            PIC  X(001).
      *
           05  AE-GEBDAT                  PIC  9(008).
           05  AE-GEBDAT-AEKZ             PIC  X(001).
      *
           05  AE-ANREDE                  PIC  9(002).
           05  AE-ANREDE-AEKZ             PIC  X(001).
      *
           05  AE-SEX                     PIC  X(001).
           05  AE-SEX-AEKZ                PIC  X(001).
      *
           05  AE-STRASSE                 PIC  X(045).
           05  AE-STRASSE-AEKZ            PIC  X(001).
      *
           05  AE-POSTFACH                PIC  X(045).
           05  AE-POSTFACH-AEKZ           PIC  X(001).
      *
           05  AE-PLZ                     PIC  X(009).
           05  AE-PLZ-AEKZ                PIC  X(001).
      *
           05  AE-ORT                     PIC  X(040).
           05  AE-ORT-AEKZ                PIC  X(001).
      *
           05  AE-AUSL                    PIC  X(003).
           05  AE-AUSL-AEKZ               PIC  X(001).
      *
           05  AE-TEL-FAX                 PIC  X(020).
           05  AE-TEL-FAX-AEKZ            PIC  X(001).
      *
           05  AE-KOMM-TYP                PIC  9(002).
           05  AE-KOMM-TYP-AEKZ           PIC  X(001).
      *
           05  AE-BUNDESL                 PIC  X(002).
           05  AE-BUNDESL-AEKZ            PIC  X(001).
      *
           05  AE-FREIBTG                 PIC  9(005)V99.
           05  AE-FREIBTG-AEKZ            PIC  X(001).
      *
           05  AE-ROLLE                   PIC  9(002).
           05  AE-ROLLE-AEKZ              PIC  X(001).
      *
           05  AE-AENDER-KZ-NAME          PIC  X(001).
           05  AE-AENDER-KZ-ADR           PIC  X(001).
           05  AE-AENDER-KZ-SONST         PIC  X(001).
120809
>>         05  AE-NACHNAME-MI             PIC  X(040).
           05  AE-NACHNAME-MI-AEKZ        PIC  X(001).
           05  AE-VORNAME-MI              PIC  X(030).
           05  AE-VORNAME-MI-AEKZ         PIC  X(001).
           05  AE-GEBDAT-MI               PIC  9(008).
           05  AE-GEBDAT-MI-AEKZ          PIC  X(001).
           05  AE-ANREDE-MI               PIC  9(002).
           05  AE-ANREDE-MI-AEKZ          PIC  X(001).
      *
      *  01 --> Herr
      *  02 --> Frau
      *  03 --> Fr鋟lein
      *  04 --> Eheleute
      *  05 --> Firma
      *  09 --> ohne Anrede
      *
           05  AE-SEX-MI           PIC  X(001).
           05  AE-SEX-MI-AEKZ      PIC  X(001).
      *
      *   M --> m鋘nlich
      *   W --> weiblich
      *   U --> unbekannt
      *
           05  AE-STRASSE-MI              PIC  X(045).
           05  AE-STRASSE-MI-AEKZ         PIC  X(001).
           05  AE-POSTFACH-MI             PIC  X(045).
           05  AE-POSTFACH-MI-AEKZ        PIC  X(001).
      *
      *
      *
           05  AE-PLZ-MI                  PIC  X(009).
           05  AE-PLZ-MI-AEKZ             PIC  X(001).
           05  AE-ORT-MI                  PIC  X(040).
           05  AE-ORT-MI-AEKZ             PIC  X(001).
           05  AE-AUSL-MI                 PIC  X(003).
           05  AE-AUSL-MI-AEKZ            PIC  X(001).
           05  AE-BANKLEITZAHL            PIC  9(008).
           05  AE-BANKLEITZAHL-AEKZ       PIC  X.
           05  AE-KONTONUMMER             PIC  9(010).
           05  AE-KONTONUMMER-AEKZ        PIC  X.
           05  AE-IBAN                    PIC  X(034).
           05  AE-IBAN-AEKZ               PIC  X.
           05  AE-BIC-SWIFT               PIC  X(011).
    <<     05  AE-BIC-SWIFT-AEKZ          PIC  X.
120809***  05  FILLER                     PIC  X(110).
110810     05  AE-NNAME-ALT               PIC  X(040).
110810     05  AE-VNAME-ALT               PIC  X(020).
310713     05  AE-ABW-BZ                  PIC  X(001).
310713     05  AE-ABW-BZ-AEKZ             PIC  X.
310713     05  AE-MAND-REFNR              PIC  X(035).
310713     05  AE-MAND-REFNR-AEKZ         PIC  X.

060312** Neues Kennzeichen, muss gesetzt werden, damit das Programm zur
      ** Aufbereitung der Kundendaten erkennt, ob f黵 diesen Kunden
      ** ein Ausgabesatz zur Kundenaktualisierung ausgegeben werden muss
060312** Beginn Deaktivierung
110810**   05  FILLER                     PIC  X(050).
060312** Ende Deaktivierung
060312     05  AE-AUSGABE-FUER-KUNDAKT    PIC  X(001).
310713     05  FILLER                     PIC  X(011).

􅅿12*
      ******************************************************************
