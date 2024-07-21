      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0665.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0665.CPv  $
      *
      *   Rev 2.1   Dec 08 2014 09:23:26   t13ex67
      *APA01809: Projekt Integration FVD    //       Migrationsprog
      *ramm
      *
      *   Rev 2.0   Mar 10 1999 12:11:24   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0665  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    05.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !05.12.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      * APA00012!13.08.98! 1.2! NEU: STRUKTURERFASSUNGS   ! PARENT   *
      *         !        !    ! UND -ÄNDERUNGSDATUM & VM  !          *
      *         !        !    !                           !          *
      * APA00012!13.08.98! 02 ! ERGÄNZT UM PEBD-DATEN (DN)! PARENT   *
      * APA01809!23.07.1402.01! PI-BEREICH GEÄNDERT       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0665                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PEST TBPA0119              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0665.
          05 PI-DRPA0665-LAST-UPDATE               PIC  X(026).
          05 PI-DRPA0665-PEST-KEY.
             10 PI-DRPA0665-PEST-ID                PIC S9(15)V  COMP-3.
          05 PI-DRPA0665-PEST-DATEN.
             10 PI-DRPA0665-PEST-AVERM             PIC S9(9) COMP.
             10 PI-DRPA0665-PEST-KD-BETR           PIC S9(9) COMP.
             10 PI-DRPA0665-PEST-VM                PIC S9(9) COMP.
             10 PI-DRPA0665-PEST-VM-GESCHFT-KZ     PIC X(1).
             10 PI-DRPA0665-KNR-PEST-ENTSTEH       PIC S9(4) COMP.
             10 PI-DRPA0665-PEST-AKZ-EVS           PIC X(1).
             10 PI-DRPA0665-PEST-AKZ-N-D-PROV      PIC X(1).
             10 PI-DRPA0665-PEST-AKZ-S-BEREIN      PIC X(1).
             10 PI-DRPA0665-PEST-AKZ-KDB-ANPAS     PIC X(1).
             10 PI-DRPA0665-PEST-STRUK-AEN-DAT     PIC  X(010).
             10 PI-DRPA0665-PEST-STRUK-DAT         PIC  X(010).
             10 PI-DRPA0665-PEST-VOR-PEST-ID       PIC S9(15)V  COMP-3.
             10 PI-DRPA0665-PEST-VSTAT             PIC X(4).
             10 PI-DRPA0665-PEST-VSTAT-UNR         PIC S9(4)  COMP.
          05 PI-DRPA0665-PEST-PEBD-DATEN.
             10 PI-DRPA0665-PEBD-DATEN-LEN         PIC S9(4) COMP.
230714*      10 PI-DRPA0665-PEBD-DATEN-TEXT     PIC X(240).
230714       10 PEBD-GRUPPE.
230714          15 FILLER                          OCCURS 20
230714                                             INDEXED BY IX-PEBD.
230714             COPY DRPA0414.
      *
       01 PO-DRPA0665.
          05 PO-DRPA0665-RC                   PIC  X(003).
             88 PO-DRPA0665-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0665-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0665-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************

