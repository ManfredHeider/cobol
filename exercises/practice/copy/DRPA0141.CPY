      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0141.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0141.CPv  $
      *
      *   Rev 2.2   18 Jul 2006 09:36:52   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.1   Jan 18 2000 14:37:20   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:27:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 10:19:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0199                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO ZWPA0026              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    22.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99!    ! NEU                       ! MUND     *
      * APA00458!24.05.06!2.2 ! PI-ZWPA0026-DRPA1658 HINZU! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWPA0026.
           05  PI-ZWPA0026-FUNKTION            PIC  X(008).
               88 PI-ZWPA0026-DRPA1261              VALUE 'DRPA1261'.
240506         88 PI-ZWPA0026-DRPA1658              VALUE 'DRPA1658'.
      *
      ******* ENDE COPY-MEMBER **************************************

