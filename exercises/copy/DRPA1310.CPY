      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1310.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA1310.CPv  $
      *
      *   Rev 2.3   Mar 22 2000 14:09:24   T13AE18
      *APA00067
      *
      *   Rev 2.2   Mar 16 2000 16:20:14   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.1   Feb 22 2000 08:42:52   T13AE18
      *APA00080
      *
      *   Rev 2.0   Nov 29 1999 13:55:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1310 FUNKTION 'LESEN DIALOGFELDER-       *
      *                             ZULÄSSIGKEIT ZUR GESELLSCHAFT'   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    03.12.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!03.12.99!2.01!NEU                        !BOHLINGER *
      * APA00078!22.03.00!2.02!PI-/PO-Bereich erweitert   !Bohlinger *
      *         !        !    !Anpassung an neue Tabelle  !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1310                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1310.
140300    05 PI-DRPA1310-PGKF-KEY.
             10 PI-DRPA1310-PGEB-GES           PIC S9(004) BINARY.
140300       10 PI-DRPA1310-KTY-KONF-ART       PIC S9(004) BINARY.
140300       10 PI-DRPA1310-KNR-KONF-ART       PIC S9(004) BINARY.

       01 PO-DRPA1310.
          05 PO-DRPA1310-RC                    PIC  X(003).
             88 PO-DRPA1310-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1310-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA1310-DATEN.
140300       10 PO-DRPA1310-KTY-KONF-REGEL      PIC S9(004) BINARY.
140300       10 PO-DRPA1310-KNR-KONF-REGEL      PIC S9(004) BINARY.
140300       10 PO-DRPA1310-PGKF-KONF-Z-INFO    PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
