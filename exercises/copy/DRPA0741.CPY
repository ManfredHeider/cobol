      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0741.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0741.CPv  $
      *
      *   Rev 2.1   12 Sep 2001 14:13:20   t13ae46
      *APA00144: Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:24   P13SP99
      *Initial revision.
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0741  FUNKTION ZU ZXPA0103                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.WANG                                           *
      *    DATUM:    14.08.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00144!14.08.01! 2.1 ! NEU                      ! WANG     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0741                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0741.
          05 PI-DRPA0741-PROGRAMM              PIC  X(008).
          05 PI-DRPA0741-UFKT                  PIC  9(002).
          05 PI-DRPA0741-DATEN.
             10 PI-DRPA0741-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0741-PMPG-ID          PIC S9(015) USAGE COMP-3.
      *
       01 PO-DRPA0741.
          05 PO-DRPA0741-RC                    PIC  X(003).
             88 PO-DRPA0741-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0741-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0741-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0741-PMPP-DATEN.
             COPY DRPA0297.
             10 PMPP-LAST-UPDATE             PIC X(026).
             10 PMPP-KENNUNG                 PIC X(008).
      *---------------------------------------------------------------*
