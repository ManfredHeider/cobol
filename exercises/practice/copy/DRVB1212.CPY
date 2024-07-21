      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1212.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1212.CPv  $
      *
      *   Rev 2.1   May 13 2019 18:10:10   T13AE30
      *AAS01285: Siegeradler: ED-Hinzurechnung bei VB 8047670, Will
      *i Sebold
      *
      *   Rev 2.0   May 13 2019 13:02:58   T13AE30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1212                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1212                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    13.05.2019                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01285!13.05.19!    ! NEU                       !T&H.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1212
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01 PI-DRVB1212.
          05 PI-DRVB1212-PROGRAMM           PIC  X(008).
          05 PI-DRVB1212-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1212-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB1212-ZEITPUNKT-VON      PIC  X(010).
          05 PI-DRVB1212-ZEITPUNKT-BIS      PIC  X(010).
      *
       01 PO-DRVB1212.
          05 PO-DRVB1212-RC                 PIC  X(003).
             88 PO-DRVB1212-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1212-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1212-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1212-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1212-DATEN.
             10 PO-DRVB1212-ED-KONTO         PIC  S9(009) COMP.
             10 PO-DRVB1212-EDD-BEZUGS-NR    PIC  S9(009) COMP.
             10 PO-DRVB1212-EDD-DAT-VON      PIC  X(010).
      *
      ******* ENDE COPY-MEMBER DRVB1212 ********************************

