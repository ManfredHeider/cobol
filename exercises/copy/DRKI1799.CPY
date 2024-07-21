      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1799.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1799.CPv  $
      *
      *   Rev 2.1   May 20 2005 13:29:18   t13ex48
      *AKI00470: Mailingaktion  TopKredit Deutsche Bank
      *
      *   Rev 2.0   May 04 2005 14:56:26   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  DEUBA-MAILING-REC.
           10 DRKI1799-KIPERSONENID               PIC  X(13).
           10 DRKI1799-KIKUNDENNUMMER             PIC S9(08) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-KIKUNDENSTATUSID           PIC  X(01).
           10 DRKI1799-KIVSABERECHDATUM           PIC  X(10).
           10 FILLER                              PIC  X(01).
           10 DRKI1799-KIANZAHLBETREUER           PIC S9(04) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-KIKUNDENAKTEVER            PIC  X(01).
           10 DRKI1799-PERSONANREDEID             PIC S9(04) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-PERSONANZVORNAME           PIC  X(30).
           10 DRKI1799-NI-PERSONANZVORNAME        PIC  X(01).
           10 DRKI1799-PERSONANZNAME              PIC  X(30).
           10 DRKI1799-NI-PERSONANZNAME           PIC  X(01).
           10 DRKI1799-PERSONGEBGRUENDDAT         PIC  X(10).
           10 FILLER                              PIC  X(01).
           10 DRKI1799-NATPHBREIHENFOLGE          PIC S9(04) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-NATPBERUFSGRID             PIC S9(04) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-NATPTITEL                  PIC S9(04) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-EINKOMONNETEINKO           PIC S9(09)V99 COMP-3.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-EINKOMONNEINKWKZ           PIC  X(01).
           10 DRKI1799-EK-LETZTEAENDERUNG         PIC  X(26).
           10 FILLER                              PIC  X(01).
           10 DRKI1799-KVBVBPERSONENID            PIC  X(13).
           10 DRKI1799-ADMASTRUKTURSTUFE          PIC S9(08) COMP.
           10 DRKI1799-NI-ADMASTRUKTURSTUFE       PIC  X(01).
           10 DRKI1799-ADMAAUSTRITTSDATUM         PIC  X(10).
           10 DRKI1799-NI-ADMAAUSTRITTSDATUM      PIC  X(01).
           10 DRKI1799-ADMADIREKTION              PIC S9(04) COMP.
           10 FILLER                              PIC  X(01).
           10 DRKI1799-PRAXISPROVSATZ             PIC S9(03)V99 COMP-3.
           10 DRKI1799-NI-PRAXISPROVSATZ          PIC  X(01).
           10 DRKI1799-MITARBEITERNR              PIC S9(09) COMP.
           10 FILLER                              PIC  X(01).
      *************** ENDE COPY DRKI1799 *****************************
