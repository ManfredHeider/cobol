      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1389.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1389.CPv  $
      *
      *   Rev 2.2   Sep 12 2002 14:43:52   t13ae46
      *APA00186: Neue Funktion DRPA1540
      *
      *   Rev 2.1   06 Jun 2000 11:48:06   T13EX21
      *APA00097 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:58:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1389                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0107                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    05.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00097!05.06.00!    ! NEU                       ! STORCH   *
      * APA00186!10.06.02!    ! DRPA1540 NEU              ! WANG     *
      *         !        !    !                           !          *
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
       01  PI-ZXPA0107.
           05  PI-ZXPA0107-FUNKTION            PIC  X(008).
               88 PI-ZXPA0107-DRPA1390              VALUE 'DRPA1390'.
100602         88 PI-ZXPA0107-DRPA1540              VALUE 'DRPA1540'.
      *
      ******* ENDE COPY-MEMBER **************************************
