      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0263.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0263.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 11:07:06   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.0   01 Apr 1998 08:38:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0224  KONSTANTEN FÜR RÜCKMELDESTATUS VOM *
      *                           PG                                 *
      *                           KTY-NR 158                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    29.05.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * PEW-REL.!29.05.98! 01 ! NEUERSTELLUNG             ! WOLFF    *
      * 2.0     !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-RM-STAT-VERTR-OBES PIC S9(004) BINARY SYNC
                                                   VALUE  0.
           05 K-KNR-RM-STAT-ZUK-ANTRAG PIC S9(004) BINARY SYNC
                                                   VALUE  4.
           05 K-KNR-RM-STAT-VERTR-UNV  PIC S9(004) BINARY SYNC
                                                   VALUE  8.
           05 K-KNR-RM-STAT-LFD-MAHN   PIC S9(004) BINARY SYNC
                                                   VALUE  12.
           05 K-KNR-RM-STAT-VERTR-STO  PIC S9(004) BINARY SYNC
                                                   VALUE  16.
           05 K-KNR-RM-STAT-VERTR-NSYS PIC S9(004) BINARY SYNC
                                                   VALUE  20.
