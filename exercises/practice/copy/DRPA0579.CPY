      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0579.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0579.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:09:56   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 13:12:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0579                                      *
      *    --------------------------------------------------------   *
      *    PVER-PVPT-TABELLE                                          *
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
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE MIT PVPT'S  *
      *    (PVPT'S).                                                 *
      *                                                              *
      *    DIE BEZEICHNUNG PVER BEZIEHT SICH AUCH AUF DIE PVPT-      *
      *    DATEN. DIE EIGENTILICH TREFFENDEN NAMEN                   *
      *                                                              *
      *       MAX-USED-PVER-PVPT        UND                          *
      *       IX-PVER-PVPT                                           *
      *                                                              *
      *    SIND ABER ZU LANG.                                        *
      *                                                              *
      ****************************************************************
      *
             20 PVER-PVPT-TABELLE.
                22 MAX-USED-PVER                    PIC S9(004) BINARY.
                22 FILLER                           OCCURS 10
                                                    INDEXED BY IX-PVER.
                   24 FILLER.
                      COPY DRPA0408.
                   24 FILLER.
                      COPY DRPA0420.
      *
      ******* ENDE COPY-MEMBER **************************************
