      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0421.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0421.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:38:52   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:34:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0421                                      *
      *    --------------------------------------------------------   *
      *    PMPT-PDPT-PDPE-TABELLE                                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.10.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE MIT PMPT'S  *
      *    (PDPT'S UND PDPE'S).                                      *
      *                                                              *
      *    DIE BEZEICHNUNG PMPT BEZIEHT SICH AUCH AUF DIE PDPT-PDPE  *
      *    DATEN. DIE EIGENTLICH TREFFENDEN NAMEN                    *
      *                                                              *
      *       MAX-USED-PMPT-PDPT-PDPE   UND                          *
      *       IX-PMPT-PDPT-PDPE                                      *
      *                                                              *
      *    SIND ABER ZU LANG.                                        *
      *                                                              *
      ****************************************************************
      *
             15 PMPT-TABELLE.
                17 MAX-USED-PMPT                    PIC S9(004) BINARY.
                17 FILLER                           OCCURS 80
                                                    INDEXED BY IX-PMPT.
                   COPY DRPA0572.
      *
      ******* ENDE COPY-MEMBER **************************************
