      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1854.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1854.CPv  $
      *
      *   Rev 2.2   Aug 30 2016 13:45:20   t13ae37
      *APA02301: PIN-Rückmelde-DTA an Central erweitern
      *
      *   Rev 2.1   Jul 29 2016 14:40:32   T13AE37
      *FPA01258: Korrektur SST-MA
      *
      *   Rev 2.0   Jul 21 2016 10:54:54   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1854                                      *
      *    --------------------------------------------------------   *
      *    init-werte für BEWEGUNGSDATEN AUS PAMS                     *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA01258!20.07.16!2.1 ! NEU                       ! BUCH     *
      * APA02301!26.08.16!2.2 ! NEU VNR-PIN ZUORDNUNG     ! BUCH     *
      *         !        !    ! FÜR CENTRAL               !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1854                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1854.
          05 PI-DRPA1854-PROGRAMM           PIC  X(008).
          05 PI-DRPA1854-UFKT               PIC  9(002).
          05 PI-DRPA1854-SEL-ART            PIC  X(001).
          05 PI-DRPA1854-DATEN.
             10 PI-DRPA1854-PGEB-GES        PIC  S9(004) BINARY.
      *
       01 PO-DRPA1854.
          05 PO-DRPA1854-RC                 PIC  X(003).
             88 PO-DRPA1854-RC-OK           VALUE 'OK '.
             88 PO-DRPA1854-RC-LEER         VALUE 'LER'.
             88 PO-DRPA1854-RC-EOF          VALUE 'EOF'.
          05 PO-DRPA1854-DATEN.
             10 PO-DRPA1854-PVER-AKT-PIN    PIC S9(009) COMP.
             10 PO-DRPA1854-PVER-VNR        PIC  X(015).
             10 PO-DRPA1854-KNR-MANDANT     PIC S9(004) COMP.
      *
      ***** ENDE-COPY-MEMBER *****************************************
 
 

