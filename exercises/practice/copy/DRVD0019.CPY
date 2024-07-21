      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0019.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVD0019.CPv  $
      *
      *   Rev 2.4   01 Nov 2007 11:44:26   t13ae23
      *FAN00783: Fehlerhaftes Copy 
      *
      *   Rev 2.3   30 Oct 2007 08:58:04   t13ae23
      *AAN00765: VM-Anteil im EGG für VBI bereitstellen
      *
      *   Rev 2.2   28 Mar 2007 15:56:52   t13ae23
      *AAN00655: Batchabläufe Täglich - Erweiterung des PS auf 3 St
      *ellen 
      *
      *   Rev 2.1   Feb 01 2005 10:33:16   t13ae23
      *AVD00007: Bereitstellung eingereichtes Geschäft.
      *
      *   Rev 2.0   Dec 17 2004 11:29:56   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00007!17.01.05! 2.1! NEU                       ! RAHNER   *
      * AAN00655!28.03.07! 2.2! DREISTELLIGE PRODUKTSCHL. ! RAHNER   *
      * AAN00765!30.10.07! 2.3! VM-KENNZEICHEN            ! RAHNER   *
      * FAN00783!01.11.07! 2.4! DATUM IN KEY              ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRVD0019.
           10  VBI-NSA                          PIC 99.
           10  VBI-NKEY.
               20  VBI-NMANR                    PIC 9(7)     COMP-3.
               20  VBI-NSTUFE                   PIC 9(3).
               20  VBI-NBMANR                   PIC 9(7)     COMP-3.
               20  VBI-NSTATDAT                 PIC 9(7)     COMP-3.
280307*        20  VBI-NSTATSCHL                PIC 9(5)     COMP-3.
280307         20  VBI-NSTATSCHL.
280307             30 VBI-NSTATSCHL-ERG-KZ      PIC 9(1).
280307             30 VBI-NSTATSCHL-PGEB-GES    PIC 9(003)    COMP-3.
280307             30 VBI-NSTATSCHL-PROD-PROD-SCHL PIC 9(003) COMP-3.
               20  VBI-KENNZ-DIVE               PIC X(1).
301007         20  VBI-KENNZ-VM                 PIC X(1).
011107         20  VBI-DATUM-TDS-SATZ           PIC 9(7)      COMP-3.
      *
      *    PRODUKTIONS-ERGEBNIS-SATZ FUER VBI.
           10  VBI-NSATZA.
               20  VBI-NEIGEN.
                    30  VBI-NEAEINH             PIC S9(8)V9     COMP-3.
                    30  VBI-NESTCK              PIC S9(7)       COMP-3.
                    30  VBI-NESUM               PIC S9(11)      COMP-3.
                    30  VBI-NEBEITR             PIC S9(11)V99   COMP-3.
               20  VBI-NGRUPPE.
                    30  VBI-NGRAEINH            PIC S9(8)V9     COMP-3.
                    30  VBI-NGRSTCK             PIC S9(7)       COMP-3.
                    30  VBI-NGRSUM              PIC S9(11)      COMP-3.
                    30  VBI-NGRBEITR            PIC S9(11)V99   COMP-3.
      *
011107*    10 VBI-DATUM-TDS-SATZ                PIC 9(7)        COMP-3.
      *
280307*    10 VBI-FILLER                        PIC X(99).
301007*    10 VBI-FILLER                        PIC X(97).
301007     10 VBI-FILLER                        PIC X(92).
      *
CCC009*    SKIP2
      ******************************************************************
      *    *** LAENGE 164 BYTES ***
      ******************************************************************

