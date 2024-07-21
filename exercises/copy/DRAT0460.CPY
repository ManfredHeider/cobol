      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0460.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAT0460.CPv  $
      *
      *   Rev 2.6   Mar 31 2015 10:55:24   t13ae37
      *APA01980: neue PGVs und DGVs  für Verarbeitung ServiceFee fü
      *r AM Leben
      *
      *   Rev 2.5   Jan 30 2015 13:29:26   t13ae37
      *APA01980: neue PGVs und DGVs  für Verarbeitung ServiceFee fü
      *r AM Leben
      *
      *   Rev 2.4   Aug 13 2014 11:34:42   t13ae39
      *APA01804: 3 neue Produktschlüssel Ventilgeschäft
      *
      *   Rev 2.3   Sep 04 2012 09:57:36   t13ae46
      *APA01447: Einführung neuer & Namensänderung bestehender Prod
      *uktschl.
      *
      *   Rev 2.2   Aug 26 2009 14:31:00   t13ae46
      *FPA01082: Korrektur Umnummerierung / Ges.91
      *
      *   Rev 2.1   Aug 10 2009 10:35:08   t13ae46
      *FPA01082: Korrektur Umnummerierung / Ges.91
      *
      *   Rev 2.0   Apr 30 2009 11:53:10   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0460  KONSTANTEN FÜR KTY = 361           *
      *                           ( PRODUKTGEBER GES.91 )            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    30.04.2009                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * FPA01082!30.04.09! 2.01! NEUERSTELLUNG             ! WANG    *
      * APA01447!03.09.12! 2.3 ! 51,56 UND 58 HINZU        ! WANG    *
      * APA01804!08.08.14! 2.4 ! 49 und 50 ergänzt         ! Strauss *
      * APA01980!22.01.15! 2.5 ! 20 GEÄNDERT               ! BUCH    *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-GEM-STRU-SACH-KFZ     PIC S9(004) BINARY VALUE 10.
220115*    05 K-KNR-GEM-STRU-LEBEN        PIC S9(004) BINARY VALUE 20.
220115     05 K-KNR-AML-SERVICE-FEE       PIC S9(004) BINARY VALUE 20.
           05 K-KNR-AML                   PIC S9(004) BINARY VALUE 21.
           05 K-KNR-AMB-GENERALI          PIC S9(004) BINARY VALUE 26.
           05 K-KNR-GENINVEST             PIC S9(004) BINARY VALUE 28.
           05 K-KNR-GEM-STRU-KOOP-PARTNER PIC S9(004) BINARY VALUE 30.
080814     05 K-KNR-HUEBNER-AG            PIC S9(004) BINARY VALUE 49.
080814     05 K-KNR-VHV-SACHVERSICHERUNG  PIC S9(004) BINARY VALUE 50.
030912     05 K-KNR-NASSAU-VERSICHERUNG   PIC S9(004) BINARY VALUE 51.
030912     05 K-KNR-SIGNAL-IDUNA          PIC S9(004) BINARY VALUE 56.
030912     05 K-KNR-MUEN-MAGD-HAGELVERS   PIC S9(004) BINARY VALUE 58.
           05 K-KNR-AXA                   PIC S9(004) BINARY VALUE 60.
           05 K-KNR-CARL-RIECK            PIC S9(004) BINARY VALUE 62.
           05 K-KNR-GOTHAER-VERS          PIC S9(004) BINARY VALUE 63.
           05 K-KNR-HAMBURGMANNHEIMER     PIC S9(004) BINARY VALUE 65.
           05 K-KNR-PROVINZIAL            PIC S9(004) BINARY VALUE 66.
           05 K-KNR-R-V-VERSICHERUNG      PIC S9(004) BINARY VALUE 68.
           05 K-KNR-SV-SPARKASSEN-VERS    PIC S9(004) BINARY VALUE 69.
           05 K-KNR-UELZENER-VERS         PIC S9(004) BINARY VALUE 71.
           05 K-KNR-VGH-VERSICHERUNG      PIC S9(004) BINARY VALUE 73.
           05 K-KNR-VICTORIA              PIC S9(004) BINARY VALUE 75.
           05 K-KNR-COFACE                PIC S9(004) BINARY VALUE 81.
           05 K-KNR-ALLIANZ               PIC S9(004) BINARY VALUE 82.
           05 K-KNR-EULER-HERMES          PIC S9(004) BINARY VALUE 84.
           05 K-KNR-VARIABLE-PROD         PIC S9(004) BINARY VALUE 90.
           05 K-KNR-BASLER-SECURITAS      PIC S9(004) BINARY VALUE 94.
 

