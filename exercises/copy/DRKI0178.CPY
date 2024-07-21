      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0178.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0178.CPv  $
      *
      *   Rev 2.2   Jan 24 2000 09:56:08   T13EX48
      *AKI00033
      *BT/PÜ
      *
      *   Rev 2.1   Sep 06 1999 12:05:38   T13EX48
      *AKI00033
      *Bestandstrennung/Pauschaluebertragung
      *
      *   Rev 2.0   Feb 27 1999 12:53:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:55:00   P13SP99
      *Initial revision.
      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00033!27.08.99!2.1 ! Erweiterung um Trenn- und ! HSE      *
      *         !        !    ! Zusammenfuehrungs-PC-Nr   !          *
      *         !        !    ! in Satzart "0"            !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
       01  DRKI0178-SATZ.
           05 DRKI0178-SATZ-SA              PIC 9(01).
           05 DRKI0178-SA0.
990927*        PC-NR des Trenn-PCs
990927       10 DRKI0178-SA0-TRENN-PC-PCNR  PIC 9(07).
990927*        PC-NR des Trenn-PCs
990927       10 DRKI0178-SA0-ZUSAMF-PC-PCNR PIC 9(07).
      *        VBNR des strukturhöchsten der zu trennenden Struktur;
             10 DRKI0178-SA0-VBNR           PIC 9(07).
      *        KZ des Geschäftsvorfalls
             10 DRKI0178-SA0-KZ             PIC 9(02).
           05 DRKI0178-SA1 REDEFINES DRKI0178-SA0.
      *        Alle zu trennenden VBs
             10 DRKI0178-SA1-VBNR           PIC 9(07).
             10 FILLER                      PIC X(16).
           05 FILLER                        PIC X(06).
      *--------    ENDE Copy-Book     DRKI0178         ----------------*
