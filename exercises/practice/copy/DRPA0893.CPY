      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0893.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0893.CPv  $
      *
      *   Rev 2.2   22 Nov 2000 10:34:02   T13EX21
      *APA00116 - Neue Funktion
      *
      *   Rev 2.1   26 Jun 2000 10:26:36   T13EX23
      *APA00067 - Neue Funktion DRPA1396
      *
      *   Rev 2.0   Feb 27 1999 14:29:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:32:18   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:49:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0893                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0095                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: EDDA SCHRIEBER                                   *
      *    DATUM:    12.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !12.03.98!01.01! NEU                      ! SCHRIEBER*
      * APA00067!16.06.00!02.02! NEUE FUNKTION DRPA1396   ! GRAY     *
      * APA00116!21.11.00!02.03! NEUE FUNKTION DRPA1434   ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
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
       01  PI-UPPA0095.
           05  PI-UPPA0095-FUNKTION            PIC  X(008).
               88 PI-UPPA0095-DRPA0894              VALUE 'DRPA0894'.
160600         88 PI-UPPA0095-DRPA1396              VALUE 'DRPA1396'.
211100         88 PI-UPPA0095-DRPA1434              VALUE 'DRPA1434'.
      *
      ******* ENDE COPY-MEMBER **************************************
