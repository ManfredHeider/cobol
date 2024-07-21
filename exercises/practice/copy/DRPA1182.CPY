      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1182.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRPA1182.CPv  $
      *
      *   Rev 2.3   Feb 03 2003 09:23:52   t13ex25
      *FPA00521; neue Funktion DRPA1557
      *
      *   Rev 2.2   Nov 26 2001 16:38:14   t13ex21
      *FPA00293 - Neue Funktion
      *
      *   Rev 2.1   Mar 16 1999 13:57:34   T01JM2S
      * 
      *
      *   Rev 2.0   Mar 05 1999 10:32:26   T01JM2S
      * 
      *
      *   Rev 1.0   21 Oct 1998 14:04:04   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1182                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWPA0117                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!05.03.99!2.1 ! NEU                       ! WOLFF    *
      * FPA00293!06.11.01!2.2 ! NEUE FUNKTION             ! STORCH   *
      * FPA00521!31.01.03!2.3 ! NEUE FUNKTION             ! JÄGER    *
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
       01  PI-ZWPA0117.
           05  PI-ZWPA0117-FUNKTION            PIC  X(008).
               88 PI-ZWPA0117-DRPA1183              VALUE 'DRPA1183'.
061101         88 PI-ZWPA0117-DRPA1374              VALUE 'DRPA1374'.
310103         88 PI-ZWPA0117-DRPA1557              VALUE 'DRPA1557'.
      *
      ******* ENDE COPY-MEMBER **************************************
