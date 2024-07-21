      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0637.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0637.cpv  $
      *
      *   Rev 2.0   May 27 1999 09:10:06   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:56:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0637  FUNKTION SELECT STANDARD-OZP ZTPA0105
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!08.12.97!01.01! NEUERSTELLUNG             ! Jäger   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0637                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0105 PNAP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0637.
          05 PI-DRPA0637-PNAP-KEY.
             10 PI-DRPA0637-PNAP-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0637-PNAP-VERSION      PIC S9(004) COMP.
      *
       01 PO-DRPA0637.
          05 PO-DRPA0637-RC                   PIC  X(003).
             88 PO-DRPA0637-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0637-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0637-LAST-UPDATE          PIC  X(026).
          05 PO-DRPA0637-PNAP.
             COPY DRPA0407.
      *
      ****** Ende Copy-Member ****************************************
