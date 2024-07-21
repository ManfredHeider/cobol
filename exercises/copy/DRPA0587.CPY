      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0587.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0587.CPv  $
      *
      *   Rev 2.3   Mar 07 2019 15:31:50   u130076
      *FPA01716: Ges 10 S145
      *
      *   Rev 2.2   Sep 24 2003 16:39:58   t13ex21
      *FPA00632 - Erhöhung Occurs auf 50
      *
      *   Rev 2.1   Nov 18 2002 16:59:34   t13ex43
      *FPA00464 : Anzahl PEST-Zeilen von 10 auf 25 erhöht
      *
      *   Rev 2.0   May 27 1999 13:22:22   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 13:13:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0587                                      *
      *    --------------------------------------------------------   *
      *    PEST-PEBD-DATEN-TABELLE                                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    31.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!31.10.97! 01 ! NEU                       ! WOLFF    *
      * FPA00464!18.11.02!2.01! OCCURS AUF 25 ERHOEHT     ! KLEEMEIER*
      * FPA00632!25.09.03!2.02! OCCURS AUF 50 ERHOEHT     ! STORCH   *
      * FPA01716!07.03.19!2.03! OCCURS AUF 100 ERHÖHT     ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE TABELLE MIT PEST'S/ *
      *    PEBD'S.                                                   *
      *                                                              *
      *    DIE BEZEICHNUNG PEST BEZIEHT SICH AUCH AUF DIE PEBD-      *
      *    DATEN. DIE EIGENTLICH TREFFENDEN NAMEN                    *
      *                                                              *
      *       MAX-USED-PEST-PPEBD       UND                          *
      *       IX-PEST-PEBD                                           *
      *                                                              *
      *    SIND ABER ZU LANG.                                        *
      *                                                              *
      ****************************************************************
      *
             20 PEST-PEBD-TABELLE.
                22 MAX-USED-PEST                    PIC S9(004) BINARY.
181102****      22 FILLER                           OCCURS 10
181102****      22 FILLER                           OCCURS 25
250903****      22 FILLER                           OCCURS 50
070319          22 FILLER                           OCCURS 100
                                                    INDEXED BY IX-PEST.
                   COPY DRPA0433.
      *
      ******* ENDE COPY-MEMBER **************************************

