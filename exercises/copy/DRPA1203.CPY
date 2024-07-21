      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1203.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1203.CPv  $
      *
      *   Rev 2.3   Jun 09 2020 11:31:10   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.2   Apr 02 2015 08:33:58   t13ex67
      *APA01986: EVS: Aufteilung Grund- und Mehrprämie
      *
      *   Rev 2.1   Apr 13 1999 10:31:06   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B
      *
      *   Rev 2.0   Mar 05 1999 09:20:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1203                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN AKT. PG-DATEN >>                   *
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
      * APA01986!28.01.15!2.2 ! PO-BEREICH UM ZAHLWEISE   ! STORCH   *
      *         !        !    ! ERWEITERN                 !          *
      * APA02908!17.02.20!2.3 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1203                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1203.
          05 PI-DRPA1203-DATEN.
170220       10 PI-DRPA1203-PGEB-GES   PIC S9(004) BINARY.
             10 PI-DRPA1203-PRINTE     PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA1203-PROD-PROD-SCHL
                                       PIC S9(004) BINARY.
      *
       01 PO-DRPA1203.
          05 PO-DRPA1203-RC            PIC  X(003).
             88 PO-DRPA1203-RC-OK           VALUE 'OK '.
             88 PO-DRPA1203-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRPA1203-DATEN.
             10 PO-DRPA1203-PMPR-PG-PROD-DATEN
                                       PIC  X(030).
             10 PO-DRPA1203-KNR-ZAHLWEISE
                                       PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


