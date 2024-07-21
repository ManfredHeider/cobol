      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1275.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1275.CPv  $
      *
      *   Rev 2.1   Jul 13 1999 17:11:16   T13AE34
      *AAN00190-Mappen DRKD0039 au KI
      *
      *   Rev 2.0   Mar 30 1999 08:44:20   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1275  ERMITTELN BEITRAG HAFT/UNFALL/SACH *
      *                 AVERM/KD.BETR. FÜR GES. AMV (10)             *
      *                 -> MAPPEN FKT. DRKD0039 AUF KI-DATENBESTAND  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00190!30.06.99!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1275                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1275.
          05 PI-DRPA1275-GES                  PIC S9(004) COMP.
          05 PI-DRPA1275-VNR                  PIC  X(015).
          05 PI-DRPA1275-STICHTAG             PIC  X(010).
      *
      *
       01 PO-DRPA1275.
          05 PO-DRPA1275-RC                   PIC  X(003).
             88 PO-DRPA1275-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1275-RC-NOT-FOUND                VALUE 'NFD'.
          05 DRPA1275-DATEN.
             10 DRPA1275-KD-BETREUER          PIC S9(007) COMP.
             10 DRPA1275-AVERM                PIC S9(007) COMP.
             10 DRPA1275-BEITRAG-HAFT         PIC S9(009)V9(002) COMP.
             10 DRPA1275-WAEHRUNG-HAFT        PIC S9(004) COMP.
             10 DRPA1275-BEITRAG-UNFALL       PIC S9(009)V9(002) COMP.
             10 DRPA1275-WAEHRUNG-UNFALL      PIC S9(004) COMP.
             10 DRPA1275-BEITRAG-SACH         PIC S9(009)V9(002) COMP.
             10 DRPA1275-WAEHRUNG-SACH        PIC S9(004) COMP.
      *
      ******** ENDE COPY-MEMBER **************************************
