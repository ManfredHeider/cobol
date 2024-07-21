      * T01JM5X   Stand: 02/10/98    10:40:22
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0475.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0475.CPv  $
      *
      *   Rev 2.4   Aug 22 2013 11:55:30   t13ex73
      *AKI01204: SEPA - DB-Online Badenia PGM/COPY
      *
      *   Rev 2.3   Oct 26 2009 07:35:48   t13ex14
      *AKI00846: Erw. der Bestands-DTAs Badenia: Programme u. Auftr
      *äge
      *
      *   Rev 2.2   Sep 28 2004 11:23:44   t13ex58
      *AKI00433: Badenia Neue KZ Eheleutevertrag Anredekz Geschlech
      *tkz im DTA
      *
      *   Rev 2.1   Apr 08 1999 15:29:58   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:06:00   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   28 Jul 1998 09:57:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 4 7 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BADENIA-DTA Kundendaten                      *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   278 Bytes                                       *
120809*    SATZLÄNGE   700 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *03.08.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
120809*AKI00846! DTA-Erweiterung f. Mitinhaber (MI)    !W.Misgeld!
      *--------!---------------------------------------!---------!-----*
310713*AKI01204! SEPA-Einführung: Erweiterung um den   !DDY      !     *
      *        ! abweichenden Beitragszahler und die   !         !     *
      *        ! Mandatsreferenznummer                 !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0475.
      *
           05  VORSATZ.
               10                     PIC  X(002).
               10  D-DAT              PIC  9(008).
               10  D-TEXT             PIC  X(010).
               10  D-BESTAND          PIC  X(017).
120809*        10                     PIC  X(241).
120809         10                     PIC  X(663).
      *
           05  KUNDEN-DATEN REDEFINES VORSATZ.
      *
               10 D-SA                PIC  X(002).
               10 D-AEND-KZ           PIC  9(002).
      *
      *  00 --> Neuanlage eines Kunden
      *  01 --> Namensänderung
      *  99 --> Stornierung
      *
               10 D-NACHNAME          PIC  X(040).
               10 D-VORNAME           PIC  X(030).
               10 D-GEBDAT            PIC  9(008).
               10 D-ANREDE-KZ         PIC  9(002).
      *
      *  01 --> Herr
      *  02 --> Frau
      *  03 --> Fräulein
      *  04 --> Eheleute
      *  05 --> Firma
      *  09 --> ohne Anrede
      *
               10 D-GESCHLECHT        PIC  X(001).
      *
      *   M --> männlich
      *   W --> weiblich
      *   U --> unbekannt
      *
               10 D-STRASSE           PIC  X(045).
               10 D-POSTFACH          PIC  X(045).
      *
      *
      *
               10 D-PLZ               PIC  X(009).
               10 D-ORT               PIC  X(040).
               10 D-AUSL-KZ           PIC  X(003).
      *
      *   internat. KFZ-KZ
      *
               10 D-TEL-FAX           PIC  X(020).
               10 D-KOMM-TYP          PIC  9(002).
      *
      *  01 --> Telefon privat
      *  02 --> Telefon geschäftlich
      *  03 --> Telefon mobil
      *  04 --> Fax privat
      *  05 --> Fax geschäftlich
      *  06 --> E-Mail privat
      *  05 --> E-Mail geschäftlich
      *
      *
               10 D-VERTRAGS-NR       PIC  9(015).
      *
               10 D-KZ-BUNDESLAND     PIC  X(002).
               10 D-FREIBETRAG        PIC  9(005)V99.
               10 D-ROLLE-KZ          PIC  9(002).
      *
               10 D-AENDER-KZ-NAME    PIC  X(001).
      *
      *  J    --> Änderung
      * SPACE --> keine Änderung
      *
               10 D-AENDER-KZ-ADR     PIC  X(001).
      *
      *  J    --> Änderung
      * SPACE --> keine Änderung
      *
               10 D-AENDER-KZ-SONST   PIC  X(001).
      *  J    --> Änderung
      * SPACE --> keine Änderung
      *
120809
>>             10 D-NACHNAME-MI       PIC  X(040).
               10 D-VORNAME-MI        PIC  X(030).
               10 D-GEBDAT-MI         PIC  9(008).
               10 D-ANREDE-KZ-MI      PIC  9(002).
      *
      *  01 --> Herr
      *  02 --> Frau
      *  03 --> Fräulein
      *  04 --> Eheleute
      *  05 --> Firma
      *  09 --> ohne Anrede
      *
               10 D-GESCHLECHT-MI     PIC  X(001).
      *
      *   M --> männlich
      *   W --> weiblich
      *   U --> unbekannt
      *
               10 D-STRASSE-MI        PIC  X(045).
               10 D-POSTFACH-MI       PIC  X(045).
      *
      *
      *
               10 D-PLZ-MI            PIC  X(009).
               10 D-ORT-MI            PIC  X(040).
               10 D-AUSL-KZ-MI        PIC  X(003).
               10 D-BANKLEITZAHL      PIC  9(008).
               10 D-KONTONUMMER       PIC  9(010).
    <<         10 D-IBAN              PIC  X(034).
120809         10 D-BIC-SWIFT         PIC  X(011).
310713         10 D-ABW-BZ            PIC  X(001).
310713         10 D-MAND-REFNR        PIC  X(035).
310713         10 FILLER              PIC  X(100).
      *
           05  NACHSATZ REDEFINES VORSATZ.
               10                     PIC  X(002).
               10  D-ANZ              PIC  9(009).
120809*        10                     PIC  X(267).
120809         10                     PIC  X(689).
      *
      ******************************************************************
