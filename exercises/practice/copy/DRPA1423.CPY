      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1423.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1423.CPv  $
      *
      *   Rev 2.4   May 23 2006 07:36:26   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.3   Mar 16 2004 12:05:50   t13ex25
      *Neue Funktion DRPA1574
      *
      *   Rev 2.2   Sep 12 2002 17:49:26   t13ex43
      *APA00206 : Neue Funktion PMBE-Haftzeit ermitteln
      *
      *   Rev 2.1   26 Sep 2000 11:05:18   T13EX21
      *APA00105 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1423                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJPA0017                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    22.09.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00105!22.09.00!2.01! NEUE FUNKTION DRPA1422    ! STORCH   *
      * APA00206!12.09.02!2.02! NEUE FUNKTION DRPA1556    ! KLEEMEIER*
      * APA00274!12.03.04!2.03! NEUE FUNKTION DRPA1574    ! JÄGER    *
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
       01  PI-ZJPA0017.
           05  PI-ZJPA0017-FUNKTION            PIC  X(008).
               88 PI-ZJPA0017-DRPA1424              VALUE 'DRPA1424'.
120902         88 PI-ZJPA0017-DRPA1556              VALUE 'DRPA1556'.
120304         88 PI-ZJPA0017-DRPA1574              VALUE 'DRPA1574'.
160506         88 PI-ZJPA0017-DRPA1657              VALUE 'DRPA1657'.
      *
      ******* ENDE COPY-MEMBER **************************************
