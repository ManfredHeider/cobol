      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3089.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3089.CPv  $
      *
      *   Rev 2.1   Nov 30 2016 13:58:18   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle
      *
      *   Rev 2.0   Aug 28 2016 16:23:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*

       01  EDISATZ.
           05  EDIEINS.
           10  EDI-MANDANT               PIC 9(07).
           10  EDI-VB-NR                 PIC 9(07).
           10  FILLER                    PIC X(01) VALUE ';'.
           10  EDI-VORGES-VB-NR          PIC 9(07).
           10  FILLER                    PIC X(01) VALUE ';'.
           05  EDIZWEI REDEFINES EDIEINS.
           10  EDI2-MANDANT              PIC 9(07).
           10  EDI2-VB-NR                PIC 9(07).
           10  FILLER                    PIC X(09) VALUE ';;'.

