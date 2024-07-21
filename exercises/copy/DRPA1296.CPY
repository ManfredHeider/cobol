      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1296.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1296.cpv  $
      *
      *   Rev 2.1   Aug 23 1999 11:45:38   T13EX21
      *APA00049 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:28   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1296                                      *
      *    --------------------------------------------------------   *
      *    PMDS-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    17.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00049!17.08.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT ALLE PMDS-DATEN FÜR ZXPA0123.         *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1296.
          05 PI-DRPA1296-PROGRAMM            PIC  X(008).
          05 PI-DRPA1296-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1296-DATEN.
             10 PI-DRPA1296-PMBE-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA1296-PMBE-VERSION     PIC S9(004) BINARY.
      *
       01 PO-DRPA1296.
          05 PO-DRPA1296-RC                  PIC  X(003).
             88 PO-DRPA1296-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1296-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA1296-RC-LEER               VALUE 'LER'.
          05 PO-DRPA1296-PMDS.
             10 PMBE-ID                      PIC S9(015) PACKED-DECIMAL.
             10 PMBE-VERSION                 PIC S9(004) BINARY.
             10 PMDS-PMBE-ID                 PIC S9(015) PACKED-DECIMAL.
             10 PMDS-PMBE-VERSION            PIC S9(004) BINARY.
             10 PMDS-GUELTIG-AB              PIC X(026).
             10 PMDS-GUELTIG-BIS             PIC X(010).
             10 PMDS-LAST-UPDATE             PIC X(026).
             10 PMDS-KENNUNG                 PIC X(008).
      *
      ******* ENDE COPY-MEMBER **************************************
