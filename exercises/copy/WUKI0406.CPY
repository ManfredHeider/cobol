      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23/04/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0406.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0406.CPv  $
      *
      *   Rev 2.1   May 06 2013 16:18:48   t13ex15
      *AKI01187: Sonderzahlungen zu LV-Verträgen / AS-Club-Wertung 
      *OZP
      *
      *   Rev 2.0   Apr 23 2013 15:41:17   T13EX15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-EINMALZAHLUNG.
           10 EMZVERTRAGSID                 PIC  X(0013).
           10 EMZVERTRAGSNR                 PIC  X(0015).
           10 EMZKOMPLETTVERT               PIC  X(0001).
           10 EMZABSCHLUSSVBNR              PIC S9(9) COMP.
           10 EMZZAHLUNGSWEISU              PIC S9(4) COMP.
           10 EMZZAHLUNGSWEISN              PIC S9(4) COMP.
           10 EMZERSTELLDATUM               PIC  X(010).
           10 EMZURSRVERTRBEG               PIC  X(010).
           10 EMZURSPVERTREND               PIC  X(010).
           10 EMZGESAMTBEITRAG              PIC S9(9)V9(2) COMP-3.
           10 EMZMONATSBEITRAG              PIC S9(9)V9(2) COMP-3.
           10 EMZLZTMONBEITRAG              PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON1                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON2                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON3                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON4                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON5                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON6                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON7                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON8                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON9                PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON10               PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON11               PIC S9(9)V9(2) COMP-3.
           10 EMZBEITRAGMON12               PIC S9(9)V9(2) COMP-3.
           10 EMZASCLUBMONLF                PIC S9(4) COMP.
           10 NIL-EMZASCLUBMONLF            PIC  X(01).
           10 EMZBEITRMONSTAND              PIC S9(4) COMP.
           10 NIL-EMZBEITRMONSTAND          PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
