      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1100.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa1100.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:17:18   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:22:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1100                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS BUFFER-OZP ZYPA0025                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    06.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.10.98! 01 ! NEU                       ! WOLFF    *
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
       01  PI-ZYPA0025.
           05  PI-ZYPA0025-FUNKTION            PIC  X(008).
               88 PI-ZYPA0025-DRPA1101              VALUE 'DRPA1101'.
      *
      ******* ENDE COPY-MEMBER **************************************
