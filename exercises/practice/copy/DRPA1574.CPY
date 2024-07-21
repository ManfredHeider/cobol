      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1574.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1574.CPv  $
      *
      *   Rev 2.2   Nov 28 2006 12:12:28   T13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Mar 16 2004 11:50:00   t13ex25
      *Neue Funktion ZJPA0017 Ermitteln Stornoeinheiten
      *
      *   Rev 2.0   Jul 04 2002 15:31:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1574 ERMITTELN DER STORNO-EINHEITEN
      *    -----------------------------------------------------------
      *    ERSTELLT: Jäger-Sokoluk
      *    DATUM:    12.03.2004
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00274!12.03.04!2.01! NEUERSTELLUNG             ! JÄGER    *
      * APA00452!17.11.06!2.02! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1574                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1574.
          05 PI-DRPA1574-DATEN.
             10 PI-DRPA1574-PGEB-GES                PIC S9(04) COMP.
             10 PI-DRPA1574-PRINTE                  PIC S9(15) COMP-3.
             10 PI-DRPA1574-FAELLIG-VON             PIC  X(10).
      *      10 PI-DRPA1574-PROD-SCHL               PIC S9(04) COMP.
             10 PI-DRPA1574-PROD-PROD-SCHL          PIC S9(04) COMP.
             10 PI-DRPA1574-GSMT-PROV               PIC S9(09)V9(02)
                                                               COMP-3.
      *
       01 PO-DRPA1574.
          05 PO-DRPA1574-RC                         PIC  X(003).
             88 PO-DRPA1574-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1574-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1574-DATEN.
             10 PO-DRPA1574-EINHEITEN               PIC S9(6)V99 COMP-3.
      ******* ENDE COPY-MEMBER ************************************** *
