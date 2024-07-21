      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1376.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1376.CPv  $
      *
      *   Rev 2.2   15 Jun 2000 10:33:26   T13EX23
      *APA00090 - Neue Funktion SQL-COMMIT
      *
      *   Rev 2.1   16 May 2000 09:20:42   T13EX23
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1376                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTPA0107                        *
      *    STANDARD-OZP                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                 *
      *    DATUM:    11.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!11.05.00! 2.1! NEUERSTELLUNG             ! GRAY     *
      * APA00090!15.06.00! 2.2! NEUE FUNKTION COMMIT      ! GRAY     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-ZTPA0107.
           05  PI-ZTPA0107-FUNKTION            PIC  X(008).
               88 PI-ZTPA0107-DRPA1377-INSERT       VALUE 'DRPA1377'.
               88 PI-ZTPA0107-DRPA1378-SELECT       VALUE 'DRPA1378'.
150600         88 PI-ZTPA0107-DRAT0228-COM          VALUE 'DRAT0228'.

      ******* ENDE COPY-MEMBER **************************************
