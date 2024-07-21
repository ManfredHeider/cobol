      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0373.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0373.CPv  $
      *
      *   Rev 2.1   Jun 07 2013 09:08:30   t13ex73
      *AKI01204: SEPA - DB-Online - Kfz
      *
      *   Rev 2.0   Mar 05 1999 11:24:22   T13EX41
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 17:03:04   P13SP13
      *Initial revision.
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KUNDEN-DATEN AMV KOMPLETTBESTAND            *
      *    ------------------------------------------------            *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    SATZLÄNGE = 300                                             *
060513*    SATZLÄNGE = 500                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *21.10.98!NEUERSTELLUNG                           !B.GOLÜCKE! 01 *
      *        !                                        !         !    *
140113*AKI01204!14.01.13! neue Felder für SEPA          !WMI      !    *
140113*        !        !                               !         !    *
      *AKI01204!06.05.13! Satzlänge angepasst           !DDY      !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI0373.

           03  KOMPL-KUNDE.

               05  KOMPL-KUN-AEND-KZ               PIC 9(002).
               05  KOMPL-KUN-NACH-NAME             PIC X(040).
               05  KOMPL-KUN-VOR-NAME              PIC X(030).
               05  KOMPL-KUN-GEB-DATUM.
                  10  KOMPL-KUN-GEB-JH             PIC 9(002).
                  10  KOMPL-KUN-GEB-DAT.
                     15  KOMPL-KUN-GEB-JJ          PIC 9(002).
                     15  KOMPL-KUN-GEB-MM          PIC 9(002).
                     15  KOMPL-KUN-GEB-TT          PIC 9(002).
               05  KOMPL-KUN-ANREDE-KZ             PIC 9(002).
               05  KOMPL-KUN-GESCHLECHT            PIC 9(001).
               05  KOMPL-KUN-STRASSE               PIC X(045).
               05  KOMPL-KUN-POSTFACH              PIC X(045).
               05  KOMPL-KUN-POST-LZ               PIC X(009).
               05  KOMPL-KUN-WOHNORT               PIC X(040).
               05  KOMPL-KUN-AUSL-KZ               PIC X(003).
               05  KOMPL-KUN-TELEFON-NR            PIC X(015).
               05  KOMPL-KUN-KOMM-KZ               PIC 9(002).
               05  KOMPL-KUN-KEY.
                   10  KOMPL-KUN-VERT-NR           PIC 9(015).
140113*        05  KOMPL-KUN-ROLLE-KZ              PIC 9(002).
140113             10  KOMPL-KUN-ROLLE-KZ          PIC 9(002).
140113*            01=VN, 02=BZ, 03=VN+BZ
               05  KOMPL-KUN-NAME-AEND-KZ          PIC X(001).
               05  KOMPL-KUN-ADRESS-AEND-KZ        PIC X(001).
               05  KOMPL-KUN-SONST-AEND-KZ         PIC X(001).
               05  KOMPL-KUN-FILLER                PIC X(001).
               05  KOMPL-KUN-TIMESTAMP             PIC X(026).
      *            JJJJ-MM-TT-hh.mm.ss.abcdef
      *            abcdef = Millisekunden (wird mit 0 geliefert)
060513*        05  FILLER                          PIC X(011).
140113         05  KOMPL-KUN-PARTNER-HOST-ID       PIC X(015).
140113         05  KOMPL-KUN-IBAN                  PIC X(034).
140113         05  KOMPL-KUN-BIC                   PIC X(011).
140113*        05  FILLER                          PIC X(055).
140113         05  FILLER                          PIC X(151).

      ******************************************************************
      *          E n d e    Copy-Member DRKI0373                       *
      ******************************************************************
