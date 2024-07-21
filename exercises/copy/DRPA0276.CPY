      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0276.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0276.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 10:21:22   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   Feb 27 1999 14:28:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:40:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:29:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0276                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UWPA0018                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00118!08.01.01! 02 ! NEUE FUNKTIONEN DRPA1454  ! MUND     *
      *         !        !    !             UND DRPA1458  !          *
      *         !        !    !             UND DRPA1463  !          *
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
       01  PI-UWPA0018.
           05  PI-UWPA0018-FUNKTION            PIC  X(008).
               88 PI-UWPA0018-DRPA0277              VALUE 'DRPA0277'.
080101         88 PI-UWPA0018-DRPA1454              VALUE 'DRPA1454'.
080101         88 PI-UWPA0018-DRPA1458              VALUE 'DRPA1458'.
080101         88 PI-UWPA0018-DRPA1463              VALUE 'DRPA1463'.
      *
      ******* ENDE COPY-MEMBER **************************************
