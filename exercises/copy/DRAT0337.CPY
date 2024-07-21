      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0337.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRAT0337.CPv  $
      *
      *   Rev 2.3   24 Apr 2001 11:10:02   t13ex21
      *APA00096 - Korrektur
      *
      *   Rev 2.2   17 Oct 2000 10:42:26   T13EX21
      *APA00096 - Korrektur
      *
      *   Rev 2.1   07 Sep 2000 15:03:50   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Mar 16 1999 09:57:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0336  KONSTANTEN FÜR DAISY               *
      *                           KONFIGURATIONSREGEL DIALOGSTATUS   *
      *                                                  KTY = 182   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GRAY                                            *
      *    DATUM:    09.08.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!09.08.00! 01 ! NEUERSTELLUNG             ! GRAY     *
      *         !17.10.00!2.01! KORREKTUR                 ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-DIA-STAT-OFFEN        PIC S9(004) BINARY VALUE 0.
           05 K-KNR-DIA-STAT-BROWSE       PIC S9(004) BINARY VALUE 8.
           05 K-KNR-DIA-STAT-GESPERRT     PIC S9(004) BINARY VALUE 9.
      *
      *    ENDE COPY-MEMBER
