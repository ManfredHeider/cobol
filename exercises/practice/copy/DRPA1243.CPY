      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1243.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1243.CPv  $
      *
      *   Rev 2.1   May 28 1999 10:36:48   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:12   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1243                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<JAHRESBEITRAG UMRECHNEN IN BEITRAG>>     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    21.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!21.05.99! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1243                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1243.
          05 PI-DRPA1243-DATEN.
             10 PI-DRPA1243-NT-JAHRESBEITRAG        PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1243-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA1243-KNR-ZAHLWEISE           PIC S9(004) BINARY.
             10 PI-DRPA1243-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA1243-FAELLIG-BIS             PIC  X(010).
             10 PI-DRPA1243-STICHTAG                PIC  X(010).
      *
       01 PO-DRPA1243.
          05 PO-DRPA1243-RC                         PIC  X(003).
             88 PO-DRPA1243-RC-OK                         VALUE 'OK '.
          05 PO-DRPA1243-DATEN.
             10 PO-DRPA1243-BEITRAG                 PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1243-ZEITRAUMFAKTOR          PIC S9(01)V9(6)
                                                    PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

