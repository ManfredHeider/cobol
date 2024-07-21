      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13/03/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0495.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0495.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:07:26   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Umsatz
      *
      *   Rev 2.0   Mar 13 2018 09:39:21   T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0495.
           10 G70UVERTRAGSNR                PIC  X(0015).
           10 G70ULFDNR                     PIC  X(0001).
           10 G70UAENDKZ                    PIC  X(0002).
           10 G70UPRODSCHL                  PIC  X(0002).
           10 G70UFREIBETR                  PIC S9(8)V9(2) COMP-3.
           10 G70UBSPGUTHAB                 PIC S9(8)V9(2) COMP-3.
           10 G70USTANDDARL                 PIC S9(8)V9(2) COMP-3.
           10 G70UKUMWOBAUPR                PIC S9(8)V9(2) COMP-3.
           10 G70UKUMANZUL                  PIC S9(8)V9(2) COMP-3.
           10 G70UMSATZTAB.
             49 G70UMSATZTAB-LEN            PIC S9(4) COMP.
             49 G70UMSATZTAB-TEXT           PIC X(105).
           10 G70UUMSWKZ                    PIC  X(0003).
           10 G70ULETZTAEND                 PIC  X(0008).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).

