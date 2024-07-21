      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0821.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0821.CPv  $
      *
      *   Rev 2.1   Feb 21 2003 12:11:24   t13ae34
      *AAN00369 - Zugriff auf Daisy-Konfiguration
      *
      *   Rev 2.0   Feb 27 1999 14:29:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Aug 1998 11:32:12   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   14 Jan 1998 12:44:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0821  FUNKTIONEN "BERECHNEN"             *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    11.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!11.02.98!2.1 ! NEU                       ! HEIDER   *
      * AAN00369!05.02.03!2.1 ! ZUGRIFF AUF DAISY-KONFIG  ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LogBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0088.
           05 PI-UPPA0088-FUNKTION            PIC X(008).
              88 PI-UPPA0088-DRPA0846             VALUE 'DRPA0846'.
              88 PI-UPPA0088-DRPA0822             VALUE 'DRPA0822'.
050203        88 PI-UPPA0088-DRPA1558             VALUE 'DRPA1558'.
