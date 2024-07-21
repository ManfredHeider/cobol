      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/04/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0540.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0540.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:38:56   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 12:02:34   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:59:20   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0540.
           10 G90CVVERTRAGSNR               PIC  X(0015).
           10 G90CVMANDANT                  PIC S9(9) COMP.
           10 G90CVAENDKZ                   PIC  X(0002).
           10 G90CVGESID                    PIC S9(4) COMP.
           10 G90CVABSCLVBNR                PIC S9(9) COMP.
           10 G90CVVEROEFF                  PIC  X(0008).
           10 G90CVSCHLIESS                 PIC  X(0008).
           10 G90CVFINBETR                  PIC S9(12)V9(2) COMP-3.
           10 G90CVZUSAGE                   PIC  X(0008).
           10 G90CVAUSZAHL                  PIC  X(0008).
           10 G90CVPRODUKTGEB               PIC  X(0030).
           10 G90CVPRODUKTART               PIC  X(0001).
           10 G90CVAKTSALDO                 PIC S9(12)V9(2) COMP-3.
           10 G90CVSOLLZINS                 PIC S9(2)V9(4) COMP-3.
           10 G90CVRATE                     PIC S9(12)V9(2) COMP-3.
           10 G90CVFAELLIG                  PIC  X(0008).
           10 G90CVRESTLAUF                 PIC S9(3)V COMP-3.
           10 G90CVDELETEKZ                 PIC  X(0001).
           10 G90CVIBAN                     PIC  X(0034).
           10 G90CVBIC                      PIC  X(0011).
           10 G90CVMANDAT                   PIC  X(0035).
           10 G90CVKTOINHNNAME              PIC  X(0030).
           10 G90CVKTOINHVNAME              PIC  X(0030).
           10 G90CVIBANKREDIT               PIC  X(0034).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).


