      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0260.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA0260.CPv  $
      *
      *   Rev 2.3   05 Feb 2001 10:21:20   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.2   Jan 18 2000 14:37:22   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.1   May 18 1999 15:11:14   T13EX22
      *Neue Funktion GEVO ermittlen
      *
      *   Rev 2.0   Feb 27 1999 14:28:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:32:40   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:27:16   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0260                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0029                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    09.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00012!12.05.99! 02 ! NEUE FUNKTION DRPA1234    ! MUND     *
      * APA00071!22.11.99! 03 ! NEUE FUNKTIONEN DRPA1016, ! MUND     *
      *         !        !    ! DRPA1032, DRPA1033,       !          *
      *         !        !    ! DRPA1259, DRAP1264        !          *
      * APA00118!06.01.01! 04 ! NEUE FUNKTIONEN DRPA1453  ! MUND     *
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
       01  PI-UPPA0029.
           05  PI-UPPA0029-FUNKTION            PIC  X(008).
               88 PI-UPPA0029-DRPA0261              VALUE 'DRPA0261'.
221199         88 PI-UPPA0029-DRPA1016              VALUE 'DRPA1016'.
221199         88 PI-UPPA0029-DRPA1032              VALUE 'DRPA1032'.
221199         88 PI-UPPA0029-DRPA1033              VALUE 'DRPA1033'.
120599         88 PI-UPPA0029-DRPA1234              VALUE 'DRPA1234'.
221199         88 PI-UPPA0029-DRPA1259              VALUE 'DRPA1259'.
221199         88 PI-UPPA0029-DRPA1264              VALUE 'DRPA1264'.
081299         88 PI-UPPA0029-DRPA1306              VALUE 'DRPA1306'.
060101         88 PI-UPPA0029-DRPA1453              VALUE 'DRPA1453'.
      *
      ******* ENDE COPY-MEMBER **************************************
