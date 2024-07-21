      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0704.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0704.cpv  $
      *
      *   Rev 2.0   May 27 1999 12:45:54   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   12 Dec 1997 09:41:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0704  FUNKTION Suche alle Einträge zu    *
      *                                    einem VSTATUS             *
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
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0704                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0704.
          05 PI-DRPA0704-PROGRAMM            PIC  X(008).
          05 PI-DRPA0704-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0704-VSTAT               PIC  X(004).
      *
       01 PO-DRPA0704.
          05 PO-DRPA0704-RC                PIC  X(003).
             88 PO-DRPA0704-RC-OK               VALUE 'OK '.
             88 PO-DRPA0704-RC-LEER             VALUE 'LER'.
             88 PO-DRPA0704-RC-EOF              VALUE 'EOF'.
          05 PO-DRPA0704-PNAP.
             COPY DRPA0407.
      *
      ****** Ende Copy-Member ****************************************
