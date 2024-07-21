      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1458.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1458.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 10:21:52   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   04 Jan 2001 11:48:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1458                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF << UPDATE NPM MIT HISTORIE >>              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    08.01.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0118!08.01.01! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1454                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1458.
          05 PI-DRPA1458-DATEN.
              10 PI-DRPA1458-PMPE-KENNUNG           PIC  X(008).
              10 PI-DRPA1458-PMPE-LAST-UPDATE       PIC  X(026).

       01 PO-DRPA1458.
          05 PO-DRPA1458-RC                         PIC  X(003).
             88 PO-DRPA1458-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1458-RC-CHANGED                    VALUE 'CHA'.

      ******* ENDE COPY-MEMBER **************************************
