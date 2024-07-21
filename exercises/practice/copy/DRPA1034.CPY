      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   Drpa1034.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\Drpa1034.cpv  $
      *
      *   Rev 2.1   Mar 16 1999 13:56:38   T01JM2S
      * 
      *
      *   Rev 2.0   Mar 05 1999 11:30:16   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:08   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1034                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0107                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.08.98! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!19.11.98!1.02! NEUE FUNKTION DRPA1126    ! BOHLINGER*
      *         !        !    !                           !          *
      * APA00012!05.03.99!2.1 ! NEUE FUNKTION DRPA1181    ! WOLFF    *
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
       01  PI-UPPA0107.
           05  PI-UPPA0107-FUNKTION            PIC  X(008).
               88 PI-UPPA0107-DRPA1035              VALUE 'DRPA1035'.
               88 PI-UPPA0107-DRAT0228              VALUE 'DRAT0228'.
               88 PI-UPPA0107-DRPA1126              VALUE 'DRPA1126'.
050399         88 PI-UPPA0107-DRPA1181              VALUE 'DRPA1181'.
      *
      ******* ENDE COPY-MEMBER **************************************
