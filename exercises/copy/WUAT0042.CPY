      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUAT0042.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WUAT0042.CPv  $
      *
      *   Rev 2.3   Sep 09 2010 12:00:38   t13ex27
      *AKI00988: Ablaufkontrolle wegen Preisakt. anpassen-Abgleich
      *
      *   Rev 2.2   Sep 08 2010 17:36:28   t13ex27
      *AKI00988: Ablaufkontrolle wegen Preisakt. anpassen-Abgleich
      *
      *   Rev 2.1   Mar 29 2010 10:29:36   t13ex27
      *AKI00935: Aktualisierung LEBENPERSONEIGEN-Abgleich
      *
      *   Rev 2.0   Mar 22 2010 13:47:50   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TBAT0042.
           10 ABKMANDANTNR                  PIC S9(9) COMP.
           10 ABKPARTGESID                  PIC S9(4) COMP.
           10 ABKJOBNAME                    PIC  X(0008).
           10 ABKLFDNR                      PIC S9(9) COMP.
           10 ABKPARTNERDATUM               PIC  X(010).
           10 ABKKATALOGDATUM               PIC  X(010).
           10 ABKBEGABLAUF                  PIC  X(026).
           10 NIL-ABKBEGABLAUF              PIC  X(01).
           10 ABKBEGAKTU                    PIC  X(026).
           10 NIL-ABKBEGAKTU                PIC  X(01).
           10 ABKENDAKTU                    PIC  X(026).
           10 NIL-ABKENDAKTU                PIC  X(01).
           10 ABKBEGEINARB                  PIC  X(026).
           10 NIL-ABKBEGEINARB              PIC  X(01).
           10 ABKENDEINARB                  PIC  X(026).
           10 NIL-ABKENDEINARB              PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 ABKABGLSTRSATZKZ              PIC  X(0001).
