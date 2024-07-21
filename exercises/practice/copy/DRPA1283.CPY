      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1283.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1283.CPv  $
      *
      *   Rev 2.2   Sep 03 2015 13:03:30   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System - MSC2
      *
      *   Rev 2.1   Jul 27 1999 10:21:42   T13AE18
      *APA00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:24   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1283                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Join-OZP ZJPA0025-PRUB/PVER/PDPT/PMPT*
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    21.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!21.07.99! 01 ! NEU                       ! BOHLINGER*
      * APA01932!21.08.15!2.2 ! NEUE FUNKTION DRPA1838    ! SCHEIDEL *
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
       01  PI-ZJPA0025.
           05  PI-ZJPA0025-FUNKTION            PIC  X(008).
               88 PI-ZJPA0025-DRPA1284              VALUE 'DRPA1284'.
               88 PI-ZJPA0025-DRPA1838              VALUE 'DRPA1838'.
      *
      ******* ENDE COPY-MEMBER **************************************

