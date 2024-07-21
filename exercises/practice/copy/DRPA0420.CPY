      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0420.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0420.CPv  $
      *
      *   Rev 2.1   24 May 2007 06:56:06   t13ex44
      *APA00631: Vertrag mit mehr als 40 Produkten
      *
      *   Rev 2.0   May 27 1999 13:37:52   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:34:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0420                                      *
      *    --------------------------------------------------------   *
      *    PVPT-DATEN-TABELLE                                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    24.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.10.97! 01 ! NEU                       ! WOLFF    *
      * APA00631!22.05.07!2.1 ! OCCURS AUF 100            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE MIT PVPT'S. *
      *                                                              *
      *    BEI ÄNDERUNG DER OCCURS-ANZAHL DIE MODULE                 *
      *                                                              *
      *       - UPPA0054                                             *
      *       - DRPA0653                                             *
      *                                                              *
      *    ANPASSEN.                                                 *
      *                                                              *
      ****************************************************************
      *
             25 PVPT-TABELLE.
                27 MAX-USED-PVPT                    PIC S9(004) BINARY.
220507*         27 FILLER                           OCCURS 40
220507          27 FILLER                           OCCURS 100
                                                    INDEXED BY IX-PVPT.
                   COPY DRPA0410.
      *
      ******* ENDE COPY-MEMBER **************************************
