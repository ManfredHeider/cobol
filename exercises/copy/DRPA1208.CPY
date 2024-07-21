      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1208.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1208.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:31:28   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Apr 13 1999 10:32:58   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B
      *
      *   Rev 2.0   Mar 30 1999 08:44:02   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1208                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN AKT. KD-BETR  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA02908!17.02.20!2.2 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1208                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1208.
          05 PI-DRPA1208-DATEN.
170220       10 PI-DRPA1208-PGEB-GES   PIC S9(004) BINARY.
             10 PI-DRPA1208-PRINTE     PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA1208.
          05 PO-DRPA1208-RC            PIC  X(003).
             88 PO-DRPA1208-RC-OK      VALUE 'OK '.
             88 PO-DRPA1208-RC-NOT-FOUND
                                       VALUE 'NFD'.
          05 PO-DRPA1208-DATEN.
             10 PO-DRPA1208-PMBE-KD-BETR
                                       PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

