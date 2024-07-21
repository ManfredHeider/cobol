      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1002.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki1002.cpv  $
      *
      *   Rev 2.2   28 Sep 2000 10:57:02   T13EX41
      *AKI00065
      *- Umstellung SAVARI auf REOB
      *
      *   Rev 2.1   May 05 2000 12:43:54   T13EX14
      *AKI000065
      *REOB-Kunden-KSDS ( Komplettbestand )
      *
      *   Rev 2.0   Feb 09 2000 14:16:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KUNDEN-DATEN AMV KOMPLETTBESTAND REOB       *
      *    -----------------------------------------------------       *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    SATZLÄNGE  300                                              *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *22.02.00!NEUERSTELLUNG                           !LENNARTZ !2.0 *
      *AKI00065!                                      !         !     *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI1002.

           03  KOMPL-KUN-R.

               05  KOMPL-KDR-AEND-KZ               PIC 9(002).
               05  KOMPL-KDR-NACH-NAME             PIC X(040).
               05  KOMPL-KDR-VOR-NAME              PIC X(030).
               05  KOMPL-KDR-GEB-DATUM.
                  10  KOMPL-KDR-GEB-JH             PIC 9(002).
                  10  KOMPL-KDR-GEB-DAT.
                     15  KOMPL-KDR-GEB-JJ          PIC 9(002).
                     15  KOMPL-KDR-GEB-MM          PIC 9(002).
                     15  KOMPL-KDR-GEB-TT          PIC 9(002).
               05  KOMPL-KDR-ANREDE-KZ             PIC 9(002).
               05  KOMPL-KDR-GESCHLECHT            PIC 9(001).
               05  KOMPL-KDR-STRASSE               PIC X(045).
               05  KOMPL-KDR-POSTFACH              PIC X(045).
               05  KOMPL-KDR-POST-LZ               PIC X(009).
               05  KOMPL-KDR-WOHNORT               PIC X(040).
               05  KOMPL-KDR-AUSL-KZ               PIC X(003).
               05  KOMPL-KDR-TELEFON-NR            PIC X(015).
               05  KOMPL-KDR-KOMM-KZ               PIC 9(002).
               05  KOMPL-KDR-KEY.
                   10  KOMPL-KDR-VERT-NR           PIC 9(015).
               05  KOMPL-KDR-ROLLE-KZ              PIC 9(002).
               05  KOMPL-KDR-NAME-AEND-KZ          PIC X(001).
               05  KOMPL-KDR-ADRESS-AEND-KZ        PIC X(001).
               05  KOMPL-KDR-SONST-AEND-KZ         PIC X(001).
               05  KOMPL-KDR-FILLER                PIC X(001).
               05  KOMPL-KDR-TIMESTAMP             PIC X(026).
      *            JJJJ-MM-TT-hh.mm.ss.abcdef
      *            abcdef = Millisekunden (wird mit 0 geliefert)
               05  KOMPL-KDR-RES-FELD              PIC X(011).

      ******************************************************************
      *          E n d e    Copy-Member DRKI1002                       *
      ******************************************************************
