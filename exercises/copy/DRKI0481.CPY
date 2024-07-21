      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0481.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0481.CPv  $
      *
      *   Rev 2.4   Aug 22 2013 11:55:40   t13ex73
      *AKI01204: SEPA - DB-Online Badenia PGM/COPY
      *
      *   Rev 2.3   Oct 26 2009 07:36:20   t13ex14
      *AKI00846: Erw. der Bestands-DTAs Badenia: Programme u. Auftr
      *äge
      *
      *   Rev 2.2   Sep 28 2004 11:23:54   t13ex58
      *AKI00433: Badenia Neue KZ Eheleutevertrag Anredekz Geschlech
      *tkz im DTA
      *
      *   Rev 2.1   Apr 08 1999 15:05:46   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:06:18   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   28 Jul 1998 09:57:34   P13SP99
      *Initial revision.
      *
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 4 8 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BADENIA Kundendaten Komplettbestand          *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   281 Bytes                                       *
120809*    SATZLÄNGE   700 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *04.08.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
120809*AKI00846! DTA-Erweiterung  f. Mitinhaber (MI)   !W.Misgeld!
      *--------!---------------------------------------!---------!-----*
310713*AKI01204! SEPA-Einführung: Erweiterung um den   !DDY      !     *
      *        ! abweichenden Beitragszahler und die   !         !     *
      *        ! Mandatsreferenznummer                 !         !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0481.
      *
           05  K-AEND-KZ         PIC  9(002).
      *
           05  K-KUNDEN-KEY.
      *
               10 K-VERTRAGS-NR  PIC  9(015).
      *
      *  00 --> Neuanlage eines Kunden
      *  01 --> Namensänderung
      *  99 --> Stornierung
      *
           05 K-NACHNAME        PIC  X(040).
           05 K-VORNAME         PIC  X(030).
           05 K-GEBDAT          PIC  9(008).
           05 K-ANREDE-KZ       PIC  9(002).
      *
      *  01 --> Herr
      *  02 --> Frau
      *  03 --> Fräulein
      *  04 --> Eheleute
      *  05 --> Firma
      *  09 --> ohne Anrede
      *
           05 K-GESCHLECHT      PIC  X(001).
      *
      *   M --> männlich
      *   W --> weiblich
      *   U --> unbekannt
      *
           05 K-STRASSE         PIC  X(045).
           05 K-POSTFACH        PIC  X(045).
      *
      *    kann das sein ??????????????????????????
      *
           05 K-PLZ             PIC  X(009).
           05 K-ORT             PIC  X(040).
           05 K-AUSL-KZ         PIC  X(003).
      *
      *   internat. KFZ-KZ
      *
           05 K-TEL-FAX         PIC  X(020).
           05 K-KOMM-TYP        PIC  9(002).
      *
      *  01 --> Telefon privat
      *  02 --> Telefon geschäftlich
      *  03 --> Telefon mobil
      *  04 --> Fax privat
      *  05 --> Fax geschäftlich
      *  06 --> E-Mail privat
      *  05 --> E-Mail geschäftlich
      *
           05 K-KZ-BUNDESLAND   PIC  X(002).
           05 K-FREIBETRAG      PIC  9(005)V99.
           05 K-ROLLE-KZ        PIC  9(002).
      *
120809     05 K-NACHNAME-MI       PIC  X(040).
>>         05 K-VORNAME-MI        PIC  X(030).
           05 K-GEBDAT-MI         PIC  9(008).
           05 K-ANREDE-KZ-MI      PIC  9(002).
      *
      *  01 --> Herr
      *  02 --> Frau
      *  03 --> Fräulein
      *  04 --> Eheleute
      *  05 --> Firma
      *  09 --> ohne Anrede
      *
           05 K-GESCHLECHT-MI     PIC  X(001).
      *
      *   M --> männlich
      *   W --> weiblich
      *   U --> unbekannt
      *
           05 K-STRASSE-MI        PIC  X(045).
           05 K-POSTFACH-MI       PIC  X(045).
      *
      *
      *
           05 K-PLZ-MI            PIC  X(009).
           05 K-ORT-MI            PIC  X(040).
    <<     05 K-AUSL-KZ-MI        PIC  X(003).
           05 K-BANKLEITZAHL      PIC  9(008).
           05 K-KONTONUMMER       PIC  9(010).
           05 K-IBAN              PIC  X(034).
           05 K-BIC-SWIFT         PIC  X(011).
310713     05 K-ABW-BZ            PIC  X(001).
310713     05 K-MAND-REFNR        PIC  X(035).
120809     05 FILLER              PIC  X(097).
           05 K-LETZTE-AEND     PIC  X(008).
      *
      **********************************************************
