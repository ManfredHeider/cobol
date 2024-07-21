      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0377.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0377.CPv  $
      *
      *   Rev 2.1   Apr 22 1999 18:43:00   T01JM2E
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:56   P13SP20
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0377  Funktion Kleinstes Verarbeitungs-  *
      *                           Datum zu Steuer-Info aus ZPKT erm. *
      *    --------------------------------------------------------- *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    15.04.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !15.04.99!01.01! NEUERSTELLUNG            ! D.WILHELM*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0377                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0377.
          05 PI-DRVB0377-DATEN.
             10 PI-DRVB0377-KNR-STEUER-INFO  PIC S9(4) BINARY.
      *
       01 PO-DRVB0377.
          05 PO-DRVB0377-RC                  PIC  X(003).
             88 PO-DRVB0377-RC-OK            VALUE 'OK '.
             88 PO-DRVB0377-RC-NOT-FOUND     VALUE 'NFD'.
          05 PO-DRVB0377-DATEN.
             10 PO-DRVB0377-MAX-STEUER-DAT   PIC X(010).
      *
      ******** Ende Copy-Member **************************************
      *---------------------------------------------------------------*
