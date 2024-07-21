      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0304.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0304.CPv  $
      *
      *   Rev 2.2   12 Jul 2000 12:04:20   T13EX21
      *APA00096 - Neue Funktion
      *
      *   Rev 2.1   Apr 04 2000 08:58:54   T13EX21
      *APA00088 - Neue Funktion DRPA1349
      *
      *   Rev 2.0   Feb 27 1999 14:28:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:17:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:22:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0304                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0017                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Schrieber                                        *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !03.06.97!    ! NEU                       ! SCHRIEBER*
      *         !        !    !                           !          *
      * APA00088!15.03.00!2.1 ! NEUE FUNKTION DRPA1349    ! STORCH   *
      * APA00096!15.06.00!2.2 ! NEUE FUNKTION DRPA1395    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZXPA0017.
           05  PI-ZXPA0017-FUNKTION            PIC  X(008).
               88 PI-ZXPA0017-DRPA0305              VALUE 'DRPA0305'.
150300         88 PI-ZXPA0017-DRPA1349              VALUE 'DRPA1349'.
150600         88 PI-ZXPA0017-DRPA1395              VALUE 'DRPA1395'.
      *
      ******* ENDE COPY-MEMBER **************************************
