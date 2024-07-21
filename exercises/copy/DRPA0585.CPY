      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0585.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0585.CPv  $
      *
      *   Rev 2.0   May 27 1999 13:21:18   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 13:12:54   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0585                                      *
      *    --------------------------------------------------------   *
      *    WORKFLOW BESTANDSMELDUNGSDATEN                             *
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
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN WORKFLOW FÜR DIE BESTANDS-        *
      *    MELDUNGSDATEN.                                            *
      *                                                              *
      *    BEI PMPR UND PMPT HANDELT ES SICH JEWEILS UM EINE         *
      *    TABELLENSTRUKTUR.                                         *
      *                                                              *
      *    DIE BEZEICHNUNG PMPT BEZIEHT SICH AUCH AUF DIE PDPT-PDPE  *
      *    DATEN. DER EIGENTLICH TREFFENDE NAME                      *
      *                                                              *
      *       PW-DRPA0585-PMPT-PDPT-PDPE                             *
      *                                                              *
      *    IST ABER ZU LANG.                                         *
      *                                                              *
      ****************************************************************
      *
       01 PW-DRPA0585.
          05 PW-DRPA0585-PMBE.
             COPY DRPA0411.
          05 PW-DRPA0585-PMPR.
             COPY DRPA0432.
          05 PW-DRPA0585-PMPT.
             COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************
