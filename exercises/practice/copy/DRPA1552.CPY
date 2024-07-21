      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1552.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1552.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:32:54   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Jul 09 2002 14:28:02   t13ex22
      *FPA00346: Neue Funktion zur PVOR-Ermittlung
      *
      *   Rev 2.0   Jul 04 2002 15:29:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1552  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    08.07.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00346!08.07.02! 2.0! NEUERSTELLUNG             ! MUND     *
      * APA02908!17.02.20!    ! STILLGELEGT               ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1552                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST PVOR <<                                         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1552.
          05 PI-DRPA1552-PROGRAMM            PIC  X(008).
          05 PI-DRPA1552-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1552-DATEN.
             10 PI-DRPA1552-PRINTE           PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA1552.
          05 PO-DRPA1552-RC                  PIC  X(003).
             88 PO-DRPA1552-RC-OK             VALUE 'OK '.
             88 PO-DRPA1552-RC-LEER           VALUE 'LER'.
             88 PO-DRPA1552-RC-EOF            VALUE 'EOF'.
          05 PO-DRPA1552-DATEN.
             10 PO-DRPA1552-PVOR-LAST-UPDATE PIC  X(026).
             10 PO-DRPA1552-PVOR-KENNUNG     PIC  X(010).
          05 PO-DRPA1552-PVOR-DATEN.
             10 PO-DRPA1552-PRINTE-VERTRAG   PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA1552-PRINTE-VORVERTRAG
                                             PIC S9(015) PACKED-DECIMAL.
      *
      ***** ENDE-COPY-MEMBER *****************************************

