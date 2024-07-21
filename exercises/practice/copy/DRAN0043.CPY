      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0043.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DRAN0043.CPv  $
      *
      *   Rev 2.15   May 28 2015 09:51:42   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.14   Apr 23 2015 13:23:42   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.13   Apr 14 2011 18:45:42   t13ae34
      *ARZ00049: Reaktivieren copy DRAN0043
      *
      *   Rev 2.12   19 May 2006 15:06:30   t13ae23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.11   10 May 2006 17:03:04   T13AE23
      *AAN00666: Erweiterung der Produktpalette um weitere Rürup-Ta
      *rife
      *
      *   Rev 2.10   Nov 14 2005 15:09:16   t13ae34
      *AAN00626: Einführung des Tarifes FRBZ
      *
      *   Rev 2.9   May 23 2005 14:10:32   t13ae34
      *AAN00583: Anpassung AML DFÜ-SST zur Auswahl 4. Fonds
      *
      *   Rev 2.8   Oct 28 2004 14:30:36   t13ae34
      *AAN00531: Neue Tariflandschaft AML 2005 im eingereichten Ges
      *chäft
      *
      *   Rev 2.7   Jun 01 2004 11:49:48   t13ae34
      *Neu Selbst. Kinderinvaliditätsversicherung und Kinderin-
      *
      *   Rev 2.6   Nov 19 2003 12:59:28   t13ae34
      *AAN00471 - Änderung SST PZM AML
      *
      *   Rev 2.5   Feb 17 2003 16:56:44   t13ae34
      *AAN00426 - Anpassung AML PZM 2.Produktpaket Risiko
      *
      *   Rev 2.4   Nov 09 2001 16:06:28   t13ae34
      *AAN00356 - PZM-DFÜ Zulagenvertrag+Nettoverfahren
      *
      *   Rev 2.3   05 Apr 2001 11:20:32   T13AE34
      *AAN00268-PZM mit Riester
      *
      *   Rev 2.2   02 Apr 2001 07:56:24   T13AE34
      *AAN00268-BUZRIS mit 2 NK
      *
      *   Rev 2.1   27 Feb 2001 11:49:54   T13AE34
      *AAN00268-PZM Antrag mit FAN00288
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:37:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      *    DIESE WERDEN UNVERÄNDERT VOM PC-DRUCKPROGRAMM ÜBER DVAG-HOST
      *    ZUM AML-HOST ÜBERTRAGEN
      ****************************************************************
      *    COPY-MEMBER: DRAN0043                                     *
      *    SCHNITTSTELLE                                             *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.12.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!12.12.00!2.1 !NEU                        !HEIDER    *
      * AAN00268!31.03.01!2.2 !PZM MIT RIESTER            !HEIDER    *
      * AAN00356!08.11.01!2.4 !ZULAGEN+NETTOVERFAHREN     !HEIDER    *
      * AAN00426!31.01.03!2.5 !2.PRODUKTPAKET AML         !HEIDER    *
      * AAN00471!18.11.03!2.6 !ANPASSUNG SST AML          !HEIDER    *
      * AAN00502!24.05.04!2.7 !ANPASSUNG KINDERINVALIDITÄT!HEIDER    *
      * AAN00531!22.10.04!2.8 !AML NEUE TARIFE NACH AEG   !HEIDER    *
      * AAN00583!23.05.05!2.9 !AML 4 FONDS                !HEIDER    *
      * AAN00626!20.10.05!2.10!AML FRBZ                   !HEIDER    *
      * AAN00666!10.05.06!2.11!NEUE RÜRUP-TARIFE          !RAHNER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0032                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
           05  PZM-PC-DATEN.
 
      *        GLEICHHEITSPRÜFUNG
               10  PZM-PC-EVNVT1      PIC  X.
               10  PZM-PC-EVNVT2      PIC  X.
240504         10  PZM-PC-EVNVT3      PIC  X.
 
      *        VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  PZM-PC-PDVNANRED   PIC  X(02).
               10  PZM-PC-PDVNTITEL   PIC  X(14).
               10  PZM-PC-PDVNVORNA   PIC  X(16).
               10  PZM-PC-PDVNZUNA    PIC  X(33).
               10  PZM-PC-PDVNZUS     PIC  X(15).
               10  PZM-PC-PDVNGBNA    PIC  X(21).
               10  PZM-PC-PDVNSTR     PIC  X(25).
               10  PZM-PC-PDVNLKZ     PIC  X(03).
               10  PZM-PC-PDVNLAND    PIC  X(35).
               10  PZM-PC-PDVNPLZ     PIC  X(06).
               10  PZM-PC-PDVNORT     PIC  X(30).
               10  PZM-PC-PDVNTELPR   PIC  X(23).
               10  PZM-PC-PDVNFAX     PIC  X(23).
               10  PZM-PC-PDVNTELDI   PIC  X(23).
               10  PZM-PC-PDVNGBDAT   PIC  X(10).
               10  PZM-PC-PDVNGBORT   PIC  X(30).
310103         10  PZM-PC-PDVNGLKZ    PIC  X(03).
310103         10  PZM-PC-PDVNGLAND   PIC  X(35).
               10  PZM-PC-PDVNFAM     PIC  X(01).
               10  PZM-PC-PDVNSELBST  PIC  X(01).
               10  PZM-PC-PDVNBER     PIC  X(40).
               10  PZM-PC-PDVNBERKY   PIC  X(06).
               10  PZM-PC-PDVNSTAAT   PIC  X(35).
               10  PZM-PC-PDVNSTAATKZ PIC  X(03).
 
      *        ERSTE VERSICHERTE PERSON
               10  PZM-PC-PDV1ANRED   PIC  X(02).
               10  PZM-PC-PDV1TITEL   PIC  X(14).
               10  PZM-PC-PDV1VORNA   PIC  X(16).
               10  PZM-PC-PDV1ZUNA    PIC  X(33).
               10  PZM-PC-PDV1ZUS     PIC  X(15).
               10  PZM-PC-PDV1GBNA    PIC  X(21).
               10  PZM-PC-PDV1STR     PIC  X(25).
               10  PZM-PC-PDV1LKZ     PIC  X(03).
               10  PZM-PC-PDV1LAND    PIC  X(35).
               10  PZM-PC-PDV1PLZ     PIC  X(06).
               10  PZM-PC-PDV1ORT     PIC  X(30).
               10  PZM-PC-PDV1GBDAT   PIC  X(10).
               10  PZM-PC-PDV1GBORT   PIC  X(30).
               10  PZM-PC-PDV1FAM     PIC  X(01).
               10  PZM-PC-PDV1SELBST  PIC  X(01).
               10  PZM-PC-PDV1BER     PIC  X(40).
               10  PZM-PC-PDV1BERKY   PIC  X(06).
               10  PZM-PC-PDV1STAAT   PIC  X(35).
               10  PZM-PC-PDV1STAATKZ PIC  X(03).
      *        ZWEITE VERSICHERTE PERSON
               10  PZM-PC-PDV2ANRED   PIC  X(02).
               10  PZM-PC-PDV2TITEL   PIC  X(14).
               10  PZM-PC-PDV2VORNA   PIC  X(16).
               10  PZM-PC-PDV2ZUNA    PIC  X(33).
               10  PZM-PC-PDV2ZUS     PIC  X(15).
               10  PZM-PC-PDV2GBNA    PIC  X(21).
               10  PZM-PC-PDV2STR     PIC  X(25).
               10  PZM-PC-PDV2LKZ     PIC  X(03).
               10  PZM-PC-PDV2LAND    PIC  X(35).
               10  PZM-PC-PDV2PLZ     PIC  X(06).
               10  PZM-PC-PDV2ORT     PIC  X(30).
               10  PZM-PC-PDV2GBDAT   PIC  X(10).
               10  PZM-PC-PDV2GBORT   PIC  X(30).
               10  PZM-PC-PDV2FAM     PIC  X(01).
               10  PZM-PC-PDV2SELBST  PIC  X(01).
               10  PZM-PC-PDV2BER     PIC  X(40).
               10  PZM-PC-PDV2BERKY   PIC  X(06).
               10  PZM-PC-PDV2STAAT   PIC  X(35).
               10  PZM-PC-PDV2STAATKZ PIC  X(03).
 
240504*>>>>>
      *        GEGEN INVALIDITÄT ZU VERSICHERNDE PERSON
               10  PZM-PC-PDV3ANRED   PIC  X(02).
               10  PZM-PC-PDV3TITEL   PIC  X(14).
               10  PZM-PC-PDV3VORNA   PIC  X(16).
               10  PZM-PC-PDV3ZUNA    PIC  X(33).
               10  PZM-PC-PDV3ZUS     PIC  X(15).
               10  PZM-PC-PDV3GBNA    PIC  X(21).
               10  PZM-PC-PDV3STR     PIC  X(25).
               10  PZM-PC-PDV3LKZ     PIC  X(03).
               10  PZM-PC-PDV3LAND    PIC  X(35).
               10  PZM-PC-PDV3PLZ     PIC  X(06).
               10  PZM-PC-PDV3ORT     PIC  X(30).
               10  PZM-PC-PDV3GBDAT   PIC  X(10).
               10  PZM-PC-PDV3GBORT   PIC  X(30).
               10  PZM-PC-PDV3FAM     PIC  X(01).
               10  PZM-PC-PDV3SELBST  PIC  X(01).
               10  PZM-PC-PDV3BER     PIC  X(40).
               10  PZM-PC-PDV3BERKY   PIC  X(06).
               10  PZM-PC-PDV3STAAT   PIC  X(35).
               10  PZM-PC-PDV3STAATKZ PIC  X(03).
240504*<<<<<
      *        VERTRIEBSDATEN / INTERNE DATEN
               10  PZM-PC-VDSPARTE    PIC  X(01).
               10  PZM-PC-VDGKZ       PIC  X(02).
      *            ABSCHLUSSVERMITTLER
               10  PZM-PC-VDABVERM    PIC  9(07)         COMP-3.
      *            PRÜFZIFFER AVERM
               10  PZM-PC-VDABVERMPZ  PIC  X(01).
               10  PZM-PC-VDVMVORNA   PIC  X(16).
               10  PZM-PC-VDVMZUNA    PIC  X(21).
      *            VM
               10  PZM-PC-VDVMNR      PIC  9(07)         COMP-3.
      *            PRÜFZIFFER VM
               10  PZM-PC-VDVMNRPZ    PIC  X(01).
      *            DVAG-PRODUKTSCHLÜSSEL
               10  PZM-PC-VDPRODKEY   PIC  9(04)         COMP-3.
      *            DVAG-ABSCHLUSSEINHEITEN
               10  PZM-PC-VDABLEIN    PIC S9(6)V9(01)    COMP-3.
181103         10  PZM-PC-VDBETREU    PIC  X(09).
               10  PZM-PC-VDDIREKT    PIC  9(03).
               10  PZM-PC-VDPIF       PIC  X(15).
               10  PZM-PC-VDPROGKZ    PIC  X(03).
 
      *        VERSICHERUNGSTECHNISCHE DATEN
 
      *        VERSIONSDATEN
181103         10  PZM-PC-VERSION     PIC  X(05).
 
      *        DATEN ZUR HAUPTVERSICHERUNG
               10  PZM-PC-HVANTNUM    PIC  X(09).
               10  PZM-PC-HVTARIF     PIC  X(15).
               10  PZM-PC-HVVSNR      PIC  9(08)         COMP-3.
               10  PZM-PC-HVVBEGINN   PIC  X(10).
               10  PZM-PC-HVN         PIC  9(03)         COMP-3.
               10  PZM-PC-HVNS        PIC  9(03)         COMP-3.
240504         10  PZM-PC-HVENDNS     PIC  X(10).
310301         10  PZM-PC-HV8ENDALT   PIC  9(03)         COMP-3.
               10  PZM-PC-HVBVNL      PIC  9(02).
               10  PZM-PC-EZAHLW      PIC  9(02).
               10  PZM-PC-HVBEITRAG   PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-HVRIS       PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-ZUEB        PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-HVVSUMME    PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-HVTAZGAR    PIC S9(09)         COMP-3.
               10  PZM-PC-TAJADF-1    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ1 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-2    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ2 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-3    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ3 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-4    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ4 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-5    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ5 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-6    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ6 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-7    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ7 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-8    PIC S9(03)         COMP-3.
201005         10  PZM-PC-VAREU-5PRZ8 PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-TAJADF-9    PIC S9(03)         COMP-3.
               10  PZM-PC-DM1FONDS    PIC  X(02).
               10  PZM-PC-DM1FONDSPROZ PIC S9(03)   COMP-3.
               10  PZM-PC-DM2FONDS     PIC  X(02).
               10  PZM-PC-DM2FONDSPROZ PIC S9(03)   COMP-3.
               10  PZM-PC-DM3FONDS     PIC  X(02).
               10  PZM-PC-DM3FONDSPROZ PIC S9(03)   COMP-3.
230505*<<<<<
               10  PZM-PC-DM4FONDS     PIC  X(02).
               10  PZM-PC-DM4FONDSPROZ PIC S9(03)   COMP-3.
230505*<<<<<
               10  PZM-PC-HVBEGREN    PIC  X(10).
               10  PZM-PC-HVREZAHLW   PIC  9(02).
               10  PZM-PC-HVGARZEIT   PIC  9(02).
201005         10  PZM-PC-HVGARZEITMAX   PIC  X(01).
               10  PZM-PC-HVRENTE     PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-HVRENTH2    PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-HVRENTH3    PIC S9(09)V9(02)   COMP-3.
081101         10  PZM-PC-HVRENTH4    PIC S9(03)         COMP-3.
081101         10  PZM-PC-HVRENTH5    PIC S9(03)         COMP-3.
               10  PZM-PC-HVKAPAB     PIC S9(09)         COMP-3.
               10  PZM-PC-HZVRENT1    PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-HZVP        PIC S9(03)         COMP-3.
               10  PZM-PC-HVRENTDMN   PIC S9(09)V9(02)   COMP-3.
310301         10  PZM-PC-AVMG        PIC  X(01).
310301         10  PZM-PC-BBG         PIC  X(01).
081101         10  PZM-PC-NVERF       PIC  X(01).
081101         10  PZM-PC-ZBE         PIC  X(01).
100506         10  PZM-PC-VFPHASEDAT  PIC  X(10).
 
 
      *        BERUFSUNFÄHIGKEITS-ZUSATZVERSICHERUNG
               10  PZM-PC-BUZKZ       PIC  X(01).
               10  PZM-PC-BERBUEA     PIC  9(02).
               10  PZM-PC-BUBARREN1   PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-BUBN        PIC  9(02).
240504         10  PZM-PC-BUZABLGEMBZHV
240504                                PIC  X(01).
               10  PZM-PC-BUBZ        PIC  9(02).
               10  PZM-PC-BUBT        PIC  9(02).
               10  PZM-PC-BUBLI       PIC  9(02).
               10  PZM-PC-BUBLR       PIC  9(02).
               10  PZM-PC-BUZI        PIC  X(01).
               10  PZM-PC-BUZP        PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-BUZR        PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-BUZD        PIC  9(02).
               10  PZM-PC-BUZRIS      PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-BUZBG       PIC  9(02).
 
      *        DYNAMIK
               10  PZM-PC-DYDYNPROZ   PIC S9(02)V9(01)   COMP-3.
221004         10  PZM-PC-DYDYND2     PIC X.
240504*>>>>>
221004*>>>>>
      *        TODESFALL-ZUSATZ
               10 PZM-PC-TZVKZ        PIC X.
               10 PZM-PC-TZVD         PIC S9(4) COMP.
               10 PZM-PC-TZVS         PIC S9(9)V9(2) COMP-3.
 
      *        HINTERBLIEBENEN-ZUSATZ
               10 PZM-PC-HZVKZ        PIC X.
               10 PZM-PC-HZVR         PIC S9(9)V9(2) COMP-3.
 
221004*<<<<<
      *        KINDERINVALIDITÄTS-ZUSATZVERSICHERUNG
               10  PZM-PC-KIZKZ       PIC  X(01).
               10  PZM-PC-KIZD        PIC  9(02).
               10  PZM-PC-KIZR        PIC  9(09)V9(02)   COMP-3.
240504*<<<<<
      *        UNFALL-ZUSATZVERSICHERUNG
               10  PZM-PC-UZVKZ       PIC  X(01).
               10  PZM-PC-UZVP        PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-UZVS        PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-UZVRIS      PIC S9(03)V9(02)   COMP-3.
 
      *        ERWERBSUNFÄHIGKEITS-ZUSATZSCHUTZ
               10  PZM-PC-EUN         PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-EUT         PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-EUZS        PIC  X(01).
               10  PZM-PC-EUZRIS      PIC S9(03)V9(02)   COMP-3.
               10  PZM-PC-EUZBG       PIC  9(02).
 
      *        DEPOT
               10  PZM-PC-DEPOT       PIC  X(01).
               10  PZM-PC-SODEPHOE    PIC S9(09)V9(02)   COMP-3.
               10  PZM-PC-SODEPZINS   PIC S9(09)V9(02)   COMP-3.
 
      *        SONSTIGE DATEN
               10  PZM-PC-GEWVA       PIC  9(01).
               10  PZM-PC-ASAMNR      PIC  9(01).
               10  PZM-PC-EKAPWR      PIC  X(01).
               10  PZM-PC-ENACHV      PIC  X(01).
               10  PZM-PC-EUMTAU      PIC  X(01).
               10  PZM-PC-EWIEDERANL  PIC  X(01).
               10  PZM-PC-BESV        PIC  X(01).
               10  PZM-PC-GESKZ       PIC  X(01).
               10  PZM-PC-EUROKZ      PIC  9(01).
310103         10  PZM-PC-UEBART      PIC  9(02).
310103         10  PZM-PC-BUZUEBART   PIC  9(02).
240504         10  PZM-PC-FIRMEINTRDATVT
240504                                PIC  X(10).
221004*>>>>>
      *        STATISTIKDATEN DVAG
               10 PZM-PC-DIVE             PIC X.
               10 PZM-PC-VDPRODKEYBUZI    PIC S9(4) COMP.
               10 PZM-PC-VDABLEINBUZI     PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGBUZI      PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYBUZR    PIC S9(4) COMP.
               10 PZM-PC-VDABLEINBUZR     PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGBUZR      PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYKIZ     PIC S9(4) COMP.
               10 PZM-PC-VDABLEINKIZ      PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGKIZ       PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYTZV     PIC S9(4) COMP.
               10 PZM-PC-VDABLEINTZV      PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGTZV       PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYHZV     PIC S9(4) COMP.
               10 PZM-PC-VDABLEINHZV      PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGHZV       PIC S9(9)V9(2) COMP-3.
 
221004*<<<<<
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 

