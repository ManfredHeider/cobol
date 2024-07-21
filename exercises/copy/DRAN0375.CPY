      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.12.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0375.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0375.CPv  $
      *
      *   Rev 2.1   Dec 08 2020 16:21:48   t13ae34
      *AAN01492: Projekt Testoptimierung im eingereichten Geschäft 
      *2020
      *
      *   Rev 2.0   Dec 07 2020 13:08:22   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0375  FUNKTION SELECT STANDARD-OZP
      *                 FÜR TBAN0026
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01492!07.12.20!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0375                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE TBAN0026 - ELEKTR_ANTRAEGE *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRAN0375.
           05  PI-DRAN0375-EA-DATEINAME          PIC  X(050).
      *
       01 PO-DRAN0375.
          05 PO-DRAN0375-RC                       PIC  X(003).
             88 PO-DRAN0375-RC-OK                      VALUE 'OK '.
             88 PO-DRAN0375-RC-NOT-FOUND               VALUE 'NFD'.
      *
          05 PO-DRAN0375-DATEN.
             10 PO-DRAN0375-EA-DATEINAME         PIC X(50).
             10 PO-DRAN0375-EA-STATUS-KTY        PIC S9(4) USAGE COMP.
             10 PO-DRAN0375-EA-STATUS-KNR        PIC S9(4) USAGE COMP.
             10 PO-DRAN0375-EA-GES-KTY           PIC S9(4) USAGE COMP.
             10 PO-DRAN0375-EA-GES-KNR           PIC S9(4) USAGE COMP.
             10 PO-DRAN0375-EA-KUNDE-NR          PIC S9(9) USAGE COMP.
             10 PO-DRAN0375-EA-MITARBEITER-NR    PIC S9(9) USAGE COMP.
             10 PO-DRAN0375-EA-AUSLESE-JOB       PIC S9(18)V USAGE
                                                             COMP-3.
             10 PO-DRAN0375-EA-O-LETZTE-AEND     PIC X(26).
             10 PO-DRAN0375-EA-VA-KZ             PIC X(1).
             10 PO-DRAN0375-EA-VERARBEITET-AM    PIC X(26).
             10 PO-DRAN0375-EA-ESIGN             PIC X(1).
             10 PO-DRAN0375-EA-PIN               PIC S9(10)V USAGE
                                                             COMP-3.
             10 PO-DRAN0375-EA-BLACK-BOX-DATA.
                15 PO-DRAN0375-EA-BB-LEN         PIC S9(4) USAGE COMP.
                15 PO-DRAN0375-EA-BB-TEXT        PIC X(500).

      ****** ENDE COPY-MEMBER ****************************************

