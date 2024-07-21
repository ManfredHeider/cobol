      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2275.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2275.CPv  $
      *
      *   Rev 2.1   Nov 19 2008 13:56:40   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz zukünftiger Beitrag
      *
      *   Rev 2.0   Oct 09 2008 09:59:34   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *      Copymember DRKI2275
      *                 AMV KFZ zukünftige Beiträge Änderungen
      *                 Satzlänge 150
      *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00731!09.10.08!    ! Neu                       ! WMI       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************

       01  DRKI2275.

           05  AE-ZKB-SATZ.

               10  AE-ZKB-VERARBKZ                      PIC  X(001).
               10  AE-ZKB-VNR                           PIC  9(015).
               10  AE-ZKB-BEITRA-ANPAS-AM               PIC  X(010).
               10  AE-ZKB-BEITRA-ANPAS-AM-AEN-KZ        PIC  X(001).
               10  AE-ZKB-BERECHNET-AM                  PIC  X(010).
               10  AE-ZKB-BERECHNET-AM-AEN-KZ           PIC  X(001).
               10  AE-ZKB-BEITR-GESAMT-NEU              PIC S9(005)V99.
               10  AE-ZKB-BEITR-GESAMT-NEU-AEN-KZ       PIC  X(001).
               10  AE-ZKB-BEITR-GESAMT-ALT              PIC S9(005)V99.
               10  AE-ZKB-BEITR-GESAMT-ALT-AEN-KZ       PIC  X(001).
               10  AE-ZKB-BEITR-TAB                     OCCURS 3.
                   15  AE-ZKB-SPARTE                    PIC  X(006).
                   15  AE-ZKB-BEITRAG-NEU               PIC S9(005)V99.
                   15  AE-ZKB-SFJ-ANZAHL-NEU            PIC  9(002).
                   15  AE-ZKB-SFJ-ANZAHL-ALT            PIC  9(002).
               10  AE-ZKB-BEITR-TAB-AEN-KZ              PIC  X(001).
               10  FILLER                               PIC  X(019).
