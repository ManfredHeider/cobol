      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0584.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0584.CPv  $
      *
      *   Rev 2.1   25 May 2000 10:24:52   T13EX21
      *APA00090 - Erweiterung um PINZ
      *
      *   Rev 2.0   May 27 1999 13:20:16   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 13:12:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0584                                      *
      *    --------------------------------------------------------   *
      *    WORKFLOW VERTRAGSDATEN                                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.10.97! 01 ! NEU                       ! WOLFF    *
      * APA00090!25.05.00!2.1 ! ERWEITERUNG UM PINZ       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN WORKFLOW FÜR DIE VERTRAGSDATEN    *
      *                                                              *
      *    BEI PVPT UND PEST-PEBD HANDELT ES SICH JEWEILS UM EINE    *
      *    TABELLENSTRUKTUR.                                         *
      *                                                              *
      ****************************************************************
      *
       01 PW-DRPA0584.
          05 PW-DRPA0584-PVER.
             COPY DRPA0408.
          05 PW-DRPA0584-PVPT.
             COPY DRPA0420.
          05 PW-DRPA0584-PEST-PEBD.
             COPY DRPA0587.
250500    05 PW-DRPA0584-PINZ.
250500       COPY DRPA1385.
      *
      ******* ENDE COPY-MEMBER **************************************
