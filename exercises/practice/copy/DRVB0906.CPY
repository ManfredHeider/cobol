      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0906.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0906.CPv  $
      *
      *   Rev 2.1   Aug 19 2008 11:34:02   t13ex13
      *ASL00304: Familien-Absicherungsplan
      *
      *   Rev 2.0   Jun 27 2008 14:57:12   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
       01 PI-DRVB0906.
          05 PI-DRVB0906-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB0906.
          05 PO-DRVB0906-RC                 PIC  X(003).
             88 PO-DRVB0906-RC-OK                VALUE 'OK '.
             88 PO-DRVB0906-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0906-DATEN.
             10 PO-DRVB0906-FAPZ-TUPEL-GESAMT    PIC  X(076).
      *

