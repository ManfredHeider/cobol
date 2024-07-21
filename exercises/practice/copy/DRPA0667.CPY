      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0667.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0667.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:27:06   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   Mar 10 1999 12:11:32   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0667  Funktion Select all Standard-OZP   *
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
      * apa00012!13.08.98! 2.0! ergänzt um PEBD-Daten (DN)! Parent   *
      * APA02908!17.02.20! 2.1! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0667                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT ALL  <<<   TABELLE PEST TBPA0119              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0667.
          05 PI-DRPA0667-PEST-KEY.
170220       10 PI-DRPA0667-PGEB-GES   PIC S9(004) BINARY.
             10 PI-DRPA0667-PEST-ID    PIC S9(015) BINARY.
      *
       01 PO-DRPA0667.
          05 PO-DRPA0667-RC            PIC  X(003).
             88 PO-DRPA0667-RC-OK           VALUE 'OK '.
             88 PO-DRPA0667-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRPA0667-PEST-DATEN.
             10 PO-DRPA0667-PEST-AVERM PIC S9(009) BINARY.
             10 PO-DRPA0667-PEST-KD-BETR
                                       PIC S9(009) BINARY.
             10 PO-DRPA0667-PEST-VM    PIC S9(009) BINARY.
             10 PO-DRPA0667-PEST-VM-GESCHFT-KZ
                                       PIC  X(001).
             10 PO-DRPA0667-KNR-PEST-ENTSTEH
                                       PIC S9(004) BINARY.
             10 PO-DRPA0667-PEST-AKZ-EVS
                                       PIC  X(001).
             10 PO-DRPA0667-PEST-AKZ-N-D-PROV
                                       PIC  X(001).
             10 PO-DRPA0667-PEST-AKZ-S-BEREIN
                                       PIC  X(001).
             10 PO-DRPA0667-PEST-AKZ-KDB-ANPAS
                                       PIC  X(001).
             10 PO-DRPA0667-PEST-STRUK-AEN-DAT
                                       PIC  X(010).
             10 PO-DRPA0667-PEST-STRUK-DAT
                                       PIC  X(010).
             10 PO-DRPA0667-PEST-VOR-PEST-ID
                                       PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0667-PEST-VSTAT PIC  X(004).
             10 PO-DRPA0667-PEST-VSTAT-UNR
                                       PIC S9(004) BINARY.
             10 PO-DRPA0667-PEST-LAST-UPDATE
                                       PIC  X(026).
             10 PO-DRPA0667-PEST-KENNUNG
                                       PIC  X(008).
          05 PO-DRPA0667-PEST-PEBD-DATEN.
             10 PO-DRPA0667-PEBD-DATEN-LEN
                                       PIC S9(004) BINARY.
             10 PO-DRPA0667-PEBD-DATEN-TEXT
                                       PIC  X(240).
      *
      ****** Ende Copy-Member ****************************************

