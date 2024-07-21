      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1085.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1085.cpv  $
      *
      *   Rev 2.1   04 Oct 1999 15:09:44   T01JM2S
      *APA00057 - EINBAU MODUS-PROTOKOLL IN PK10
      *
      *   Rev 2.0   May 27 1999 12:02:28   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:21:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1085                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZXPA0010                          *
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
      * APA00057!01.10.99!2.1 ! NEUE FUNKTION DRPA1215    ! WOLFF    *
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
       01  PI-ZXPA0010.
           05  PI-ZXPA0010-FUNKTION            PIC  X(008).
               88 PI-ZXPA0010-DRPA1086              VALUE 'DRPA1086'.
               88 PI-ZXPA0010-DRPA1087              VALUE 'DRPA1087'.
               88 PI-ZXPA0010-DRPA1088              VALUE 'DRPA1088'.
               88 PI-ZXPA0010-DRPA1089              VALUE 'DRPA1089'.
               88 PI-ZXPA0010-DRPA1090              VALUE 'DRPA1090'.
011099         88 PI-ZXPA0010-DRPA1215              VALUE 'DRPA1215'.
      *
      ******* ENDE COPY-MEMBER **************************************
