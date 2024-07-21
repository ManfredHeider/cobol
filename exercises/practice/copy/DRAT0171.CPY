      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0171.CPv  $
      *
      *   Rev 2.1   22 May 2000 14:25:44   T13EX44
      *Weitere Zustaende
      *
      *   Rev 2.0   Jun 19 1999 12:31:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   05 Sep 1997 09:20:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0171  KONSTANTEN FÜR                     *
      *                           VERTRAGSSTAND            KTY = 138 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.10.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * APA00090!12.05.00! 2.1! WEITERE ZUSTAENDE         ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-UNBEKANNT-VERTR-STAND   PIC S9(004) BINARY VALUE 1.
           05 K-KNR-VERTR-NICHT-VORHANDEN   PIC S9(004) BINARY VALUE 2.
           05 K-KNR-VERTR-UNVOLLSTAENDIG    PIC S9(004) BINARY VALUE 3.
           05 K-KNR-VERTR-VOLLSTAENDIG      PIC S9(004) BINARY VALUE 4.
