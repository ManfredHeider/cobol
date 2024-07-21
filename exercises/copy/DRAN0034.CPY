      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0034.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAN0034.CPv  $
      *
      *   Rev 2.2   03 May 2001 12:52:22   t13ae34
      *FAN00304-doppelte PIN bei PZM
      *
      *   Rev 2.1   07 Mar 2001 09:55:16   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:36   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:36:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0034  FUNKTIONEN STANDARD-OZP            *
      *                           PZMA TBAN0002                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00268!03.05.01!2.2 ! NEUE FKT. DRAN0046        ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTAN0002.
           03 PI-ZTAN0002-FUNKTION            PIC X(008).
              88 PI-ZTAN0002-DRAN0035-INS         VALUE 'DRAN0035'.
              88 PI-ZTAN0002-DRAN0036-UPD         VALUE 'DRAN0036'.
              88 PI-ZTAN0002-DRAN0037-DEL         VALUE 'DRAN0037'.
              88 PI-ZTAN0002-DRAN0038-SEL-VN      VALUE 'DRAN0038'.
              88 PI-ZTAN0002-DRAN0039-SEL-VN-AV   VALUE 'DRAN0039'.
              88 PI-ZTAN0002-DRAN0040-SEL-PIN     VALUE 'DRAN0040'.
              88 PI-ZTAN0002-DRAN0046-SEL-PIN-A   VALUE 'DRAN0046'.
