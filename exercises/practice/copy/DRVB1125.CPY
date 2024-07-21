      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.11.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1125.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1125.CPv  $
      *
      *   Rev 2.1   Nov 25 2015 13:34:10   t13ae30
      *AVB01376: Neue Funktion $34c u. §34i f. Badenia
      *
      *   Rev 2.0   Nov 06 2015 14:53:08   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01376!06.11.15!2.1 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1125                                 *
      *    ERMITTELN MA-BEFBED-NEU                                   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1125.
          05 PI-DRVB1125-DATEN.
             10 PI-DRVB1125-MA-ST-NR         PIC S9(009) COMP.
      *
       01 PO-DRVB1125.
          05 PO-DRVB1125-RC                  PIC  X(003).
             88 PO-DRVB1125-RC-OK            VALUE 'OK '.
             88 PO-DRVB1125-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB1125-DATEN.
             10 PO-DRVB1125-MA-34C-DAT              PIC  X(10).
             10 PO-DRVB1125-MA-34C-AMT              PIC  X(40).
             10 PO-DRVB1125-MA-34C-STRASSE          PIC  X(40).
             10 PO-DRVB1125-MA-34C-PLZ              PIC  X(09).
             10 PO-DRVB1125-MA-34C-ORT              PIC  X(40).
             10 PO-DRVB1125-MA-34I-DAT              PIC  X(10).
             10 PO-DRVB1125-MA-34I-AMT              PIC  X(40).
             10 PO-DRVB1125-MA-34I-STRASSE          PIC  X(40).
             10 PO-DRVB1125-MA-34I-PLZ              PIC  X(09).
             10 PO-DRVB1125-MA-34I-ORT              PIC  X(40).
             10 PO-DRVB1125-MA-34I-REG-NR           PIC  X(15).

