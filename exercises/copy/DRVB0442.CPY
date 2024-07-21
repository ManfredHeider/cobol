      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0442.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0442.CPv  $
      *
      *   Rev 2.1   Aug 24 1999 17:37:52   T13EX19
      *fki00122 - neue fkt für ZWVB0004
      *
      *   Rev 2.0   Aug 04 1999 10:30:12   P13SP20
      *Archiverstellung
      *---------------------------------------------------------------*
     ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !24.08.99! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0422                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0442.
          05 PI-DRVB0442-DATEN.
             10 PI-DRVB0442-MA-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0442.
          05 PO-DRVB0442-RC                  PIC  X(003).
             88 PO-DRVB0442-RC-OK            VALUE 'OK '.
             88 PO-DRVB0442-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0442-DATEN.
             10 PO-DRVB0442-KNR-MA-TYP       PIC S9(4) COMP.
      ******** Ende Copy-Member **************************************
