      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0191.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0191.CPv  $
      *
      *   Rev 2.2   Jul 24 2007 12:18:08   t13ae34
      *AAN00764: Funktion Setzen Antragstatus
      *
      *   Rev 2.1   Jul 24 2007 09:24:58   t13ae34
      *AAN00764: Funktion Setzen Antragstatus
      *
      *   Rev 2.0   Jul 18 2007 14:59:32   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0191  FUNKTION UPDATE ANTRAGSTATUS       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00764!20.07.07!2.2 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0191                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  ANTRAGSTATUS  <<<   TABELLE ANTA TBAN0008    *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0191.
          05 PI-DRAN0191-LAST-UPDATE           PIC  X(026).

          05 PI-DRAN0191-KEY.
             35 PGEB-GES                       PIC S9(4) COMP.
             35 ANTA-NR                        PIC S9(15)V COMP-3.

          05 PI-DRAN0191-DATEN.
             35 ANTA-ANTR-STATUS-1             PIC S9(4) COMP.
             35 ANTA-STATUS-DAT-1              PIC X(10).
             35 ANTA-ANTR-STATUS-2             PIC S9(4) COMP.
             35 ANTA-STATUS-DAT-2              PIC X(10).
             35 ANTA-ANTR-STATUS-3             PIC S9(4) COMP.
             35 ANTA-STATUS-DAT-3              PIC X(10).
             35 ANTA-DTA-STRUK-DAT             PIC X(10).
      *
       01 PO-DRAN0191.
          05 PO-DRAN0191-RC                    PIC  X(003).
             88 PO-DRAN0191-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0191-RC-CHANGED                VALUE 'CHA'.
             88 PO-DRAN0191-RC-NOT-FOUND              VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
