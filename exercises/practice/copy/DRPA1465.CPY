      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1465.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1465.CPv  $
      *
      *   Rev 2.3   Apr 18 2005 09:07:24   t13ex44
      *APA00341: Umstellung Provisionsbelieferung Central von CVIS 
      *auf KOMPAS
      *
      *   Rev 2.2   09 Feb 2001 14:45:12   T13EX22
      *APA00118: Zahlweise auf Pic X(2) geändert
      *
      *   Rev 2.1   05 Feb 2001 10:22:06   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   09 Jan 2001 10:12:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1465                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ZAHLWEISE NORMIEREN >>                   *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!21.01.01! 01 ! NEU                       ! MUND     *
      * APA00341!22.02.05! 2.3! PG-ZAHLWEISE NUMMERISCH   ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1465                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1465.
          05 PI-DRPA1465-DATEN.
             10 PI-DRPA1465-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA1465-PG-ZAHLWEISE     PIC  X(002).
220205       10 PI-DRPA1465-PG-ZAHLWEISE-NUM
220205          REDEFINES PI-DRPA1465-PG-ZAHLWEISE
220205                                       PIC  9(002).
      *
       01 PO-DRPA1465.
          05 PO-DRPA1465-RC                  PIC  X(003).
             88 PO-DRPA1465-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1465-RC-UNZ                        VALUE 'UNZ'.
          05 PO-DRPA1465-KNR-ZAHLWEISE       PIC S9(004)  BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

