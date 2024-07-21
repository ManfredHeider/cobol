      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0315.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/WUKI0315.CPv  $
      *
      *   Rev 2.16   Nov 24 2020 09:06:10   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.15   Nov 12 2015 13:17:20   t13ex27
      *AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect.-Abgl
      *eich
      *
      *   Rev 2.14   Oct 22 2015 10:18:40   t13ex27
      *FKI01319: Wartungsauftrag Abgleich
      *
      *   Rev 2.13   Nov 24 2010 10:32:40   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.12   Oct 26 2010 10:51:02   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.11   Oct 01 2010 11:59:00   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.10   Nov 16 2009 11:57:58   t13ex27
      *AKI00897: DB-Änderung für Rel. 4.66
      *
      *   Rev 2.9   Aug 12 2009 12:06:20   t13ex27
      *AKI00843: DB-Änderungen für Rel. 4.60-DWS
      *
      *   Rev 2.8   Oct 12 2006 13:35:26   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE-PMKI0079
      *
      *   Rev 2.7   Oct 18 2004 09:44:20   t13ex27
      *AKI00436: Datenbankänderungen in KI für Fix 67 EA-Programme
      *
      *   Rev 2.6   Jun 11 2003 13:11:20   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0315.
           10 GHPVERTRAGSID                 PIC  X(0013).
           10 GHPGUTHABPRODLFDNR            PIC S9(4) COMP.
           10 GHPPRODUKTID                  PIC S9(4) COMP.
           10 GHPANLAGENAME.
             49 GHPANLAGENAME-LEN           PIC S9(4) COMP.
             49 GHPANLAGENAME-TEXT          PIC X(15).
           10 NIL-GHPANLAGENAME             PIC  X(01).
           10 GHPSCHLAUSZGESCH              PIC S9(9)V9(2) COMP-3.
           10 NIL-GHPSCHLAUSZGESCH          PIC  X(01).
           10 GHPSCHLAUSZGWKZ               PIC  X(0001).
           10 GHPZINSSATZ                   PIC S9(3)V9(2) COMP-3.
           10 NIL-GHPZINSSATZ               PIC  X(01).
           10 GHPINFOWPERREKZ               PIC  X(0001).
           10 GHPINFOSPERREDATUM            PIC  X(010).
           10 NIL-GHPINFOSPERREDATUM        PIC  X(01).
           10 GHPVERFUEGUNGID               PIC  X(0002).
           10 NIL-GHPVERFUEGUNGID           PIC  X(01).
           10 GHPGUTHABENANT                PIC S9(6)V9(3) COMP-3.
           10 NIL-GHPGUTHABENANT            PIC  X(01).
           10 GHPGUTHABENWERT               PIC S9(9)V9(2) COMP-3.
           10 NIL-GHPGUTHABENWERT           PIC  X(01).
           10 GHPGUTHWERTWKZ                PIC  X(0001).
           10 GHPGUTHABEN                   PIC S9(9)V9(2) COMP-3.
           10 NIL-GHPGUTHABEN               PIC  X(01).
           10 GHPGUTHABENWKZ                PIC  X(0001).
           10 GHPSUBTYPE                    PIC  X(0001).
           10 NIL-GHPSUBTYPE                PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 GHPRISIKOKLASSE               PIC S9(4) COMP.
           10 NIL-GHPRISIKOKLASSE           PIC  X(01).
           10 GHPEIGENAUFWAND               PIC S9(9)V9(2) COMP-3.
           10 NIL-GHPEIGENAUFWAND           PIC  X(01).
           10 GHPFAXREVERS                  PIC  X(0001).
           10 GHPTELEFONREVERS              PIC  X(0001).
           10 GHPONLINEREVERS               PIC  X(0001).
           10 GHPZWISCHENGEWINN             PIC S9(9)V9(2) COMP-3.
           10 NIL-GHPZWISCHENGEWINN         PIC  X(01).
           10 GHPZWISCHENGEWWKZ             PIC  X(0001).
           10 GHPPERFLFDJAHR                PIC S9(13)V9(5) COMP-3.
           10 NIL-GHPPERFLFDJAHR            PIC  X(01).
           10 GHPPERFGESAMT                 PIC S9(13)V9(5) COMP-3.
           10 NIL-GHPPERFGESAMT             PIC  X(01).
           10 GHPEIGENVERMITTELT            PIC  X(0001).
           10 GHPOFFNUNGSDATUM              PIC  X(010).
           10 NIL-GHPOFFNUNGSDATUM          PIC  X(01).
           10 GHPSCHLIESSDATUM              PIC  X(010).
           10 NIL-GHPSCHLIESSDATUM          PIC  X(01).
           10 GHPANLAGEZIEL                 PIC  X(0001).
           10 GHPABSCHLUSSVBNR              PIC S9(9) COMP.
           10 NIL-GHPABSCHLUSSVBNR          PIC  X(01).
           10 GHPDEPOTART                   PIC  X(0001).
           10 NIL-GHPDEPOTART               PIC  X(01).
           10 GHPPERFDAT                    PIC  X(010).
           10 NIL-GHPPERFDAT                PIC  X(01).
           10 GHPBRPERFLFJBEG               PIC  X(010).
           10 NIL-GHPBRPERFLFJBEG           PIC  X(01).
           10 GHPBRPERFLFJEND               PIC  X(010).
           10 NIL-GHPBRPERFLFJEND           PIC  X(01).
           10 GHPBRPERFGESBEG               PIC  X(010).
           10 NIL-GHPBRPERFGESBEG           PIC  X(01).
           10 GHPBRPERFGESEND               PIC  X(010).
           10 NIL-GHPBRPERFGESEND           PIC  X(01).
           10 GHPPORTFOLIONR                PIC  X(0010).
           10 NIL-GHPPORTFOLIONR            PIC  X(01).
           10 GHPDETAILNAM-KTY              PIC S9(4) COMP.
           10 NIL-GHPDETAILNAM-KTY          PIC  X(01).
           10 GHPDETAILNAM-KNR              PIC S9(15)V COMP-3.
           10 NIL-GHPDETAILNAM-KNR          PIC  X(01).
           10 GHPNRGWINKLBBA                PIC S9(11)V9(2) COMP-3.
           10 NIL-GHPNRGWINKLBBA            PIC  X(01).
           10 GHPNRGWINKLBBAVH              PIC S9(6)V9(2) COMP-3.
           10 NIL-GHPNRGWINKLBBAVH          PIC  X(01).
           10 GHPNRGWOHNEBBA                PIC S9(11)V9(2) COMP-3.
           10 NIL-GHPNRGWOHNEBBA            PIC  X(01).
           10 GHPNRGWOHNEBBAVH              PIC S9(6)V9(2) COMP-3.
           10 NIL-GHPNRGWOHNEBBAVH          PIC  X(01).
           10 GHPRGWUDEPOT                  PIC S9(9)V9(2) COMP-3.
           10 NIL-GHPRGWUDEPOT              PIC  X(01).
           10 GHPPROVISIONKZ                PIC  X(0001).
           10 NIL-GHPPROVISIONKZ            PIC  X(01).
           10 GHPSONDERKONDI                PIC  X(0001).
           10 NIL-GHPSONDERKONDI            PIC  X(01).
           10 GHPWPHGDAT                    PIC  X(010).
           10 NIL-GHPWPHGDAT                PIC  X(01).
           10 GHPEMAILADRESSE.
             49 GHPEMAILADRESSE-LEN         PIC S9(4) COMP.
             49 GHPEMAILADRESSE-TEXT        PIC X(128).
           10 NIL-GHPEMAILADRESSE           PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 GHPBLZKUNDEN                  PIC  X(0008).
           10 NIL-GHPBLZKUNDEN              PIC  X(01).
           10 GHPESAFE                      PIC  X(0001).
