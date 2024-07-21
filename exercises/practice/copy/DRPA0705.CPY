      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0705.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0705.CPv  $
      *
      *   Rev 2.1   Jun 18 1999 11:02:30   T13EX21
      *APA00012 - PI-Bereich geändert
      *
      *   Rev 2.0   May 27 1999 12:46:14   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   12 Dec 1997 09:41:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0705  FUNKTION Suche alle Einträge zu    *
      *                                    einem Zustand             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    15.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01! NEUERSTELLUNG             ! Jäger   *
      * APA00012!17.06.99!01.02! PI-KTY-PNAP-ZUSTAND ENTF. ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0705                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0705.
          05 PI-DRPA0705-PROGRAMM            PIC  X(008).
          05 PI-DRPA0705-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0705-KNR-PNAP-ZUSTAND    PIC S9(004) BINARY.
170699*   05 PI-DRPA0705-KTY-PNAP-ZUSTAND    PIC S9(004) BINARY.
      *
       01 PO-DRPA0705.
          05 PO-DRPA0705-RC                PIC  X(003).
             88 PO-DRPA0705-RC-OK               VALUE 'OK '.
             88 PO-DRPA0705-RC-LEER             VALUE 'LER'.
             88 PO-DRPA0705-RC-EOF              VALUE 'EOF'.
          05 PO-DRPA0705-PNAP.
             COPY DRPA0407.
      *
      ****** Ende Copy-Member ****************************************
