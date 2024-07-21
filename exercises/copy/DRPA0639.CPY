      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0639.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0639.CPv  $
      *
      *   Rev 2.0   May 27 1999 11:13:52   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:56:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0639                                      *
      *    --------------------------------------------------------   *
      *    PMDS-DATEN-TABELLE                                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    20.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!20.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE MIT PMDS'S. *
      *                                                              *
      *    BEI ÄNDERUNG DER OCCURS-ANZAHL DIE COPY-STRECKE           *
      *                                                              *
      *       - DRPA0615                                             *
      *                                                              *
      *    ANPASSEN.                                                 *
      *                                                              *
      ****************************************************************
      *
             25 PMDS-TABELLE.
                27 MAX-USED-PMDS                    PIC S9(004) BINARY.
                27 FILLER                           OCCURS 10
                                                    INDEXED BY IX-PMDS.
                   COPY DRPA0570.
      *
      ******* ENDE COPY-MEMBER **************************************
