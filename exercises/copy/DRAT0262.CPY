      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0262.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0262.CPv  $
      *
      *   Rev 2.1   Oct 12 2016 08:14:04   t13ae37
      *APA02316: Einheitenmaximierung
      *
      *   Rev 2.0   May 28 1999 18:08:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   05 Sep 1997 09:20:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0262  KONSTANTEN FÜR                     *
      *                           KZ-EH-MAX                KTY = 157 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    06.05.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!06.05.98! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * APA02316!13.09.16! 2.1! EINH-MAX UMGEHEN          ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-KZ-EH-MAX-VOLL       PIC S9(004) BINARY VALUE 1.
           05 K-KNR-KZ-EH-MAX-GEKUERZT   PIC S9(004) BINARY VALUE 2.
           05 K-KNR-KZ-EH-MAX-ABGESCHN   PIC S9(004) BINARY VALUE 3.
           05 K-KNR-KZ-EH-MAX-UMGEHEN    PIC S9(004) BINARY VALUE 4.

