      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0344.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/WUKI0344.CPv  $
      *
      *   Rev 2.13   Mar 30 2020 08:43:14   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.12   Nov 24 2010 10:32:48   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.11   Oct 26 2010 10:51:20   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.10   Oct 01 2010 11:59:06   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.9   Jul 21 2009 13:35:32   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.8   Apr 30 2009 08:17:56   t13ex14
      *AKI00805: OZP-Objekte
      *
      *   Rev 2.7   Feb 23 2009 07:36:04   t13ex14
      *AKI00731: Migration der Tabelle VERSBEDINGUNGBEZ f. Kfz
      *
      *   Rev 2.6   Jul 03 2003 09:35:12   t13ex27
      *FKI00748 - DB-Änderung aus AKI00106
      *
      *   Rev 2.5   25 Jul 2001 10:33:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0344.
           10 VBBVERTRAGSID                 PIC  X(0013).
           10 VBBVERSTARLFDNR               PIC S9(4) COMP.
           10 VBBVERSBEDINGUNGID            PIC S9(9) COMP.
           10 VBBVERSBEDINGLFDNR            PIC S9(4) COMP.
           10 VBBVEBPARTNERGES              PIC S9(4) COMP.
           10 VBBVERSBEDINGKEY              PIC  X(0002).
           10 NIL-VBBVERSBEDINGKEY          PIC  X(01).
           10 VBBVERSBEDINGUNG.
             49 VBBVERSBEDINGUNG-LEN        PIC S9(4) COMP.
             49 VBBVERSBEDINGUNG-TEXT       PIC X(50).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 VBBBEDINGUNG-KTY              PIC S9(4) COMP.
           10 NIL-VBBBEDINGUNG-KTY          PIC  X(01).
           10 VBBBEDINGUNG-KNR              PIC S9(15)V COMP-3.
           10 NIL-VBBBEDINGUNG-KNR          PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 VBBAVB-KTY                    PIC S9(4) COMP.
           10 NIL-VBBAVB-KTY                PIC  X(01).
           10 VBBAVB-KNR                    PIC S9(15)V COMP-3.
           10 NIL-VBBAVB-KNR                PIC  X(01).

