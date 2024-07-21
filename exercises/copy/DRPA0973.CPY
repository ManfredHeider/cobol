      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0973.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0973.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:37:54   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER: DRPA0973                                      *
      *   --------------------------------------------------------   *
      *   FUNKTIONEN DES BUFFER-OZP'S ZYPA0016                       *
      *   ( BISHER BENUTZT VON PGPA0023 IM DIALOG PO21               *
      *     - PROVISIONSBESTAND - AUSKUNFT PROVISIONSMELDUNGEN )     *
      *   -------------------------------------------------------    *
      *   ERSTELLT: KUEKENHOEHNER                                    *
      *   DATUM:    20.06.1998                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.06.98! NEU                            ! KUEKEN   *
      *         !18.01.99! NEUe funktion drpa1154         !bohlinger *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZYPA0016.
           05  PI-ZYPA0016-FUNKTION            PIC  X(008).
      *----ERGEBNISMENGE X BEREITSTELLEN
               88 PI-ZYPA0016-DRPA0974              VALUE 'DRPA0974'.
      *----ERGEBNISMENGE y BEREITSTELLEN
               88 PI-ZYPA0016-DRPA1154              VALUE 'DRPA1154'.
      *
      ******* ENDE COPY-MEMBER **************************************
