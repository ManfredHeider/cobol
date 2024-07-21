      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.02.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1073.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1073.CPv  $
      *
      *   Rev 2.1   May 22 2014 10:19:52   t13ae13
      *ASL00509: FAP Dialogänderung Vb 31
      *
      *   Rev 2.0   Feb 04 2014 10:38:34   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
       01 PI-DRVB1073.
          05 PI-DRVB1073-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB1073.
          05 PO-DRVB1073-RC                 PIC  X(003).
             88 PO-DRVB1073-RC-OK                VALUE 'OK '.
             88 PO-DRVB1073-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1073-DATEN.
             10 PO-DRVB1073-FAPZ-TUPEL-GESAMT    PIC  X(135).
      *

