      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1557.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1557.CPv  $
      *
      *   Rev 2.2   Nov 10 2006 08:29:42   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Feb 03 2003 09:26:26   t13ex25
      *FPA00521; Neue Funktion Ermitteln Einheiten Vertrag/Produkt
      *
      *   Rev 2.0   Jul 04 2002 15:29:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1557                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN AKTUELLE EINHEITEN>>               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JÄGER                                           *
      *    DATUM:    31.01.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00521!31.01.03!2.1 ! NEU                       ! JÄGER    *
      * APA00452!01.11.06!2.2 ! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1557                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1557.
          05 PI-DRPA1557-DATEN.
             10 PI-DRPA1557-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA1557-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
      *      10 PI-DRPA1557-PROD-SCHL               PIC S9(004) BINARY.
             10 PI-DRPA1557-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PI-DRPA1557-FAELLIG-DAT             PIC  X(010).
      *
       01 PO-DRPA1557.
          05 PO-DRPA1557-RC                         PIC  X(003).
             88 PO-DRPA1557-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1557-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1557-DATEN.
             10 PO-DRPA1557-EINHEITEN               PIC S9(006)V99
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

