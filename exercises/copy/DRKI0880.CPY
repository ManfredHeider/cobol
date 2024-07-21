      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0880.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0880.CPv  $
      *
      *   Rev 2.1   Jul 13 1999 17:11:12   T13AE34
      *AAN00190-Mappen DRKD0039 au KI
      *
      *   Rev 2.0   Apr 29 1999 13:57:08   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0880  ERMITTELN BEITRAG HAFT/UNFALL/SACH *
      *                           JE VERTRAG                         *
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
      *    FÜR DIE FUNKTION DRKI0880                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0880.
          05 PI-DRKI0880-GES                  PIC S9(004) COMP.
          05 PI-DRKI0880-VNR                  PIC  X(015).
      *
      *
       01 PO-DRKI0880.
          05 PO-DRKI0880-RC                   PIC  X(003).
             88 PO-DRKI0880-RC-OK                       VALUE 'OK '.
             88 PO-DRKI0880-RC-NOT-FOUND                VALUE 'NFD'.
          05 DRKI0880-DATEN.
             10 DRKI0880-BEITRAG-HAFT         PIC S9(009)V9(002) COMP.
             10 DRKI0880-BEITRAG-UNFALL       PIC S9(009)V9(002) COMP.
             10 DRKI0880-BEITRAG-SACH         PIC S9(009)V9(002) COMP.
      *
      *         INHALT | WÄHRUNG
      *         'D'    | DM
      *         ' '    | DM
      *         'E'    | EURO
      *         'X'    | UNBEKANNT
             10 DRKI0880-WKZ                  PIC  X(001).
      *
      ******** ENDE COPY-MEMBER **************************************
