      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1187.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1187.CPv  $
      *
      *   Rev 2.2   Oct 23 2019 09:57:16   t13ae37
      *APA02885: Ges. 16 - Einführung 4 neue PGVs 
      *
      *   Rev 2.1   Mar 16 1999 14:05:34   T01JM2S
      *
      *
      *   Rev 2.0   Mar 05 1999 10:34:50   T01JM2S
      *
      *
      *   Rev 1.0   21 Oct 1998 14:04:32   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1187                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWPA0111                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    08.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA02885!16.10.19!2.2 ! LESE MAX BEGINN FÜR PRINTE! BUCH     *
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
       01  PI-ZWPA0111.
           05  PI-ZWPA0111-FUNKTION            PIC  X(008).
               88 PI-ZWPA0111-DRPA1188              VALUE 'DRPA1188'.
161019         88 PI-ZWPA0111-DRPA1940              VALUE 'DRPA1940'.
      *
      ******* ENDE COPY-MEMBER **************************************

