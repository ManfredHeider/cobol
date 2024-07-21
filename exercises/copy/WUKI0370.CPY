      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0370.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:09:56   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung OZP
      *
      *   Rev 2.0   Apr 19 2016 14:07:54   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
        05  WU-REISEVERSICHRUNG.
           10 RVSVERTRAGSID                 PIC  X(0013).
           10 RVSVETLFDNR                   PIC S9(4) COMP.
           10 RVSEINZVTGHOSTID              PIC  X(0015).
           10 RVSBEDINGUNG-KTY              PIC S9(4) COMP.
           10 NIL-RVSBEDINGUNG-KTY          PIC  X(01).
           10 RVSBEDINGUNG-KNR              PIC S9(15)V COMP-3.
           10 NIL-RVSBEDINGUNG-KNR          PIC  X(01).
           10 RVSVSUMRGEPCK                 PIC S9(9)V9(2) COMP-3.
           10 NIL-RVSVSUMRGEPCK             PIC  X(01).
           10 RVSVSUMRUECK                  PIC S9(9)V9(2) COMP-3.
           10 NIL-RVSVSUMRUECK              PIC  X(01).
           10 RVSMAILING-KTY                PIC S9(4) COMP.
           10 NIL-RVSMAILING-KTY            PIC  X(01).
           10 RVSMAILING-KNR                PIC S9(15)V COMP-3.
           10 NIL-RVSMAILING-KNR            PIC  X(01).
           10 RVSZUSINFO-KTY                PIC S9(4) COMP.
           10 NIL-RVSZUSINFO-KTY            PIC  X(01).
           10 RVSZUSINFO-KNR                PIC S9(15)V COMP-3.
           10 NIL-RVSZUSINFO-KNR            PIC  X(01).
           10 RVSINFOQUELLE-KTY             PIC S9(4) COMP.
           10 NIL-RVSINFOQUELLE-KTY         PIC  X(01).
           10 RVSINFOQUELLE-KNR             PIC S9(15)V COMP-3.
           10 NIL-RVSINFOQUELLE-KNR         PIC  X(01).
           10 RVSDIFDECKKZ                  PIC  X(0001).
           10 RVSGESELLSCH-KTY              PIC S9(4) COMP.
           10 NIL-RVSGESELLSCH-KTY          PIC  X(01).
           10 RVSGESELLSCH-KNR              PIC S9(15)V COMP-3.
           10 NIL-RVSGESELLSCH-KNR          PIC  X(01).
           10 RVSVERSNUMMER                 PIC  X(0040).
           10 NIL-RVSVERSNUMMER             PIC  X(01).
           10 RVSVUMSTELLUNGDAT             PIC  X(010).
           10 NIL-RVSVUMSTELLUNGDAT         PIC  X(01).
           10 RVSKUENDIGUNGKZ               PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).

