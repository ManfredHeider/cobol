      * T13EX47   Stand: 17/11/98    16:21:37
      * T13EX39   Stand: 21/07/98    21:34:29
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0377.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0377.CPv  $
      *
      *   Rev 2.2   Oct 13 2004 14:08:04   t13ex39
      *AKI00249: DEPOTSTAMM -BESTAND
      *
      *   Rev 2.1   Jul 10 2003 13:41:58   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   Feb 27 1999 12:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:37:42   T13EX39
      *FUNKTION FUER UPKI0041
      *
      *   Rev 1.0   14 Jul 1998 17:03:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00327!11.03.03! 1.0!     FÜR DEUTSCGE BANK MIT ! WHO      *
      *         !        !    !     BANKPRODUKTEN         !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00249!15.09.04! 1.1!     WPKNR mit aufgenommen ! WHO      *
      *         !        !    !     für PMKI0723          !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI0377.
          05 PI-DRKI0377-PZVP-VERSION          PIC S9(004) BINARY.
          05 PI-DRKI0377-PROGRAMM              PIC  X(008).
          05 PI-DRKI0377-UFKT                  PIC  9(002).
          05 PI-DRKI0377-SCHALTER              PIC  9.
          05 PI-DRKI0377-DATEN.
            10 PI-DRKI0377-FEHLERSATZ.
             15 PI-DRKI0377-FUNKTION              PIC  X(010).
             15 PI-DRKI0377-TABELLE               PIC  X(020).
             15 PI-DRKI0377-SQL-FEHLER            PIC  X(004).
             15 PI-DRKI0377-KEY                   PIC  X(040).
             15 FILLER REDEFINES PI-DRKI0377-KEY.
                25 PI-DRKI0377-GES                PIC  9(002).
                25 PI-DRKI0377-VNR                PIC  X(015).
110303*         25 FILLER                         PIC  X(023).
110303          25 FILLER                         PIC  X(001).
110303          25 PI-DRKI0377-LFDNR              PIC  9(004).
150904          25 FILLER                         PIC  X(001).
150904          25 PI-DRKI0377-WPKNR              PIC  X(006).
150904          25 FILLER                         PIC  X(011).
150904*         25 FILLER                         PIC  X(018).

      *
       01 PO-DRKI0377.
          05 PO-DRKI0377-RC                    PIC  X(003).
             88 PO-DRKI0377-RC-OK                     VALUE 'OK '.
             88 PO-DRKI0377-RC-NOK                    VALUE 'NOK'.
      *
