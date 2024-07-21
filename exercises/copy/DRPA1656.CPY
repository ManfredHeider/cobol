      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1656.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1656.CPv  $
      *
      *   Rev 2.3   Jul 05 2018 09:06:34   t13ex44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.2   Apr 25 2017 07:17:30   t13ex44
      *FPA01568: Korrektur Setzen K112
      *
      *   Rev 2.1   Jul 17 2006 12:29:10   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.0   Mar 07 2006 08:05:20   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1656 EINBUCHEN VB-UE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    14.03.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00409!14.03.06! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * FPA01568!12.03.17! 2.2! MANDANT HINZU             ! SCHEIDEL *
      * APA02579!02.07.18! 2.3! BEW-HERKUNFT HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1656                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  EINBUCHEN EINES VB-UE IN EINE STRUKTUR  <<<          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1656.
           05 PI-DRPA1656-KNR-MANDANT  PIC S9(004)       BINARY.
           05 PI-DRPA1656-PGEB-GES     PIC S9(004)       BINARY.
           05 PI-DRPA1656-VNR          PIC  X(015).
020718     05 PI-DRPA1656-KNR-BEW-HRKFT
020718                                 PIC S9(004)       BINARY.
      *
        01 PO-DRPA1656.
           05 PO-DRPA1656-RC           PIC  X(003).
              88 PO-DRPA1656-RC-OK     VALUE 'OK '.
              88 PO-DRPA1656-RC-NOT-FOUND
                                       VALUE 'NFD'.
              88 PO-DRPA1656-RC-KEIN-KI
                                       VALUE 'KKI'.
           05 PO-DRPA1656-VBUE-GUELTIG-AB
                                       PIC  X(010).
           05 PO-DRPA1656-STRK-AEND-KZ PIC  X(001).
 

