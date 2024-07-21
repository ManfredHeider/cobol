      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0767.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0767.CPv  $
      *
      *   Rev 2.1   Mar 24 2005 16:08:16   T13AE27
      *FAS00154: Jahres-WBW 2005 Wien
      *
      *   Rev 2.0   Mar 24 2005 12:05:24   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0767                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0767.
          05 PI-DRVB0767-BETR               PIC  S9(009) COMP.
          05 PI-DRVB0767-KNR-WB-TYP         PIC  S9(004) COMP.
          05 PI-DRVB0767-KNR-WERTUNGS-TYP   PIC  S9(004) COMP.
          05 PI-DRVB0767-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0767-DAT-VON            PIC  X(010).
          05 PI-DRVB0767-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0767.

          05 PO-DRVB0767-RC                 PIC  X(003).
             88 PO-DRVB0767-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0767-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0767-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0767-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0767-DATEN.
             10 PO-DRVB0767-PARTEH-EH      PIC S9(009)V9 COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0748 *****************************
