      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0970.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0970.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:25:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0970 DATENFELDER FüR DIALOG PO13         *
      *                          'VERTRAGSPRODUKTDATEN'              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!15.06.98! 01 !NEU                        !STORCH    *
      *         !08.02.99!1.2 !AVERM/KDBETR ENTFäLLT      !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FüR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG PO13                      *
      *                                                              *
      ****************************************************************

      ***    LISTFELDER
             35 PVPT-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.
             35 PVPT-VERSION                     PIC S9(004) BINARY.
             35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
             35 PVPT-VT-PRIO                     PIC S9(004) BINARY.
             35 PVPT-PPFL-BEGINN                 PIC  X(010).
             35 PVPT-PIN                         PIC S9(009) BINARY.
080299***    35 PVER-AVERM-KDBETR                PIC S9(007) BINARY.
             35 PVPT-BEWERT-BETRAG               PIC S9(009)V9(2)
                                                 PACKED-DECIMAL.
             35 PVPT-KNR-WAEHRUNG                PIC  X(004).
             35 PVPT-F-GUELTIG-AB                PIC  X(010).
