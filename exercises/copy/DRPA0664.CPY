      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0664.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0664.CPv  $
      *
      *   Rev 2.0   Mar 10 1999 12:11:20   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0664  Funktion Insert Standard-OZP       *
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
      * apa00012!13.08.98! 1.2! neu: Strukturerfassungs   ! Parent   *
      *         !        !    ! und -änderungsdatum & VM  !          *
      * apa00012!13.08.98! 02 ! ergänzt um PEBD-Daten (DN)! Parent   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0664                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PEST TBPA0119              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0664.
          05 PI-DRPA0664-PROGRAMM              PIC  X(008).
          05 PI-DRPA0664-UFKT                  PIC  9(002).
          05 PI-DRPA0664-PEST-DATEN.
             10 PI-DRPA0664-PEST-ID          PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0664-PEST-AVERM          PIC S9(9) USAGE COMP.
             10 PI-DRPA0664-PEST-KD-BETR        PIC S9(9) USAGE COMP.
130898       10 PI-DRPA0664-PEST-VM             PIC S9(9) USAGE COMP.
             10 PI-DRPA0664-PEST-VM-GESCHFT-KZ  PIC X(1).
             10 PI-DRPA0664-KNR-PEST-ENTSTEH    PIC S9(4) USAGE COMP.
             10 PI-DRPA0664-PEST-AKZ-EVS        PIC X(1).
             10 PI-DRPA0664-PEST-AKZ-N-D-PROV   PIC X(1).
             10 PI-DRPA0664-PEST-AKZ-S-BEREIN   PIC X(1).
             10 PI-DRPA0664-PEST-AKZ-KDB-ANPAS  PIC X(1).
130898       10 PI-DRPA0664-PEST-STRUK-AEN-DAT  PIC  X(010).
130898       10 PI-DRPA0664-PEST-STRUK-DAT      PIC  X(010).
             10 PI-DRPA0664-PEST-VOR-PEST-ID PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0664-PEST-VSTAT PIC X(4).
             10 PI-DRPA0664-PEST-VSTAT-UNR      PIC S9(4) USAGE COMP.
130898    05 PI-DRPA0664-PEST-PEBD-DATEN.
130898       10 PI-DRPA0664-PEBD-DATEN-LEN      PIC S9(4) COMP.
130898       10 PI-DRPA0664-PEBD-DATEN-TEXT     PIC X(240).
      *
       01 PO-DRPA0664.
          05 PO-DRPA0664-RC                   PIC  X(003).
             88 PO-DRPA0664-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0664-RC-DUPLICATE-VALUE            VALUE 'DVA'.

      ****** Ende Copy-Member ****************************************
