      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0578.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:/MF/CPY/DRPA0578.CPv  $
      *
      *   Rev 2.0   12 Sep 2001 15:07:44   t13ae46
      *APA00144: Version neu
      *
      *   Rev 1.1   12 Sep 2001 14:11:34   t13ae46
      *APA00144: Neuerstellung
      *
      *   Rev 1.0   22 Oct 1997 13:12:24   P13PVCS
      *Initial revision.
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0578                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE ÜBER PGEB UND PDTA-ID>>     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. WANG                                          *
      *    DATUM:    13.08.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00144!13.08.01! 1.0 ! NEU                      ! WANG     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0578                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0578.
          05 PI-DRPA0578-PROGRAMM            PIC  X(008).
          05 PI-DRPA0578-UFKT                PIC  9(002).
          05 PI-DRPA0578-DATEN.
             10 PI-DRPA0578-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0578-PDTA-ID          PIC S9(015) USAGE COMP-3 .
      *
       01 PO-DRPA0578.
          05 PO-DRPA0578-RC                  PIC  X(003).
             88 PO-DRPA0578-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0578-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0578-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0578-PMPE-DATEN.
             COPY DRPA0289.
             10 PMPE-LAST-UPDATE             PIC X(026).
             10 PMPE-KENNUNG                 PIC X(008).

      *---------------------------------------------------------------*
