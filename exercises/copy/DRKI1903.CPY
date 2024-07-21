      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1903.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1903.CPv  $
      *
      *   Rev 2.1   Jul 17 2006 12:27:56   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.0   May 19 2006 14:37:10   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1903  LESEN VB-UE AUS KUNDENUEBERTRAGUNG *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    14.03.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00409!14.03.06! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1903                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN VBNR ZU GES, VNR, VBNR, KDU-ART  <<<           *
      *                                                              *
      ****************************************************************
        01 PI-DRKI1903.
           05 PI-DRKI1903-PGEB-GES     PIC S9(004)       BINARY.
           05 PI-DRKI1903-VNR          PIC  X(015).
           05 PI-DRKI1903-VBNR         PIC S9(009)       BINARY.
      *
        01 PO-DRKI1903.
           05 PO-DRKI1903-RC           PIC  X(003).
              88 PO-DRKI1903-RC-OK     VALUE 'OK '.
              88 PO-DRKI1903-RC-NOT-FOUND
                                       VALUE 'NFD'.
           05 PO-DRKI1903-GUELTIG-AB   PIC  X(010).
      *
