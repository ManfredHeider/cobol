      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0358.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0358.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:23:10   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Apr 09 1997 16:48:00   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0358                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS BUFFER-OZP ZYPA0004                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    08.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.07.97! 01 ! NEU                       ! D.WILHELM*
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
       01  PI-ZYPA0004.
           05  PI-ZYPA0004-FUNKTION            PIC  X(008).
               88 PI-ZYPA0004-DRPA0359              VALUE 'DRPA0359'.
               88 PI-ZYPA0004-DRPA0496              VALUE 'DRPA0496'.
      *
      ******* ENDE COPY-MEMBER **************************************
