      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0265.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0265.CPv  $
      *
      *   Rev 2.0   26 Jul 1999 10:49:06   T01JM2S
      *APA00012 - NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 08:38:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0265  KONSTANTEN FÜR ERMITTLUNG EVS      *
      *                           KTY-NR 169                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                           *
      *    DATUM:    16.06.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !16.06.98! 01 ! NEUERSTELLUNG             ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-EVS-ERM-KDBETR-KI  PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-EVS-ERM-KDBETR-PM  PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-EVS-ERM-KDBETR-PEW PIC S9(004) BINARY SYNC
                                                   VALUE  3.
           05 K-KNR-EVS-ERM-KDBETR-FEH PIC S9(004) BINARY SYNC
                                                   VALUE  4.
           05 K-KNR-EVS-ERM-AVERM-PEW  PIC S9(004) BINARY SYNC
                                                   VALUE  10.
           05 K-KNR-EVS-ERM-AVERM-PM   PIC S9(004) BINARY SYNC
                                                   VALUE  11.
           05 K-KNR-EVS-ERM-AVERM-FEHL PIC S9(004) BINARY SYNC
                                                   VALUE  12.
           05 K-KNR-EVS-ERM-HIN-KEINVT PIC S9(004) BINARY SYNC
                                                   VALUE  20.
           05 K-KNR-EVS-ERM-HIN-KEINAM PIC S9(004) BINARY SYNC
                                                   VALUE  21.
