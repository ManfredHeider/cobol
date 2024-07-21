      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU512S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU512S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:00:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   4    14.09.95 16:52:40
      ****************************************************************
      *                                                              *
      *          UU512S - COPYMEMBER                                 *
      *          ======                                              *
      *                                                              *
      *          TDS-SAETZE ONLINE-VERARBEITUNG   O P A L            *
      *          FESTE SATZLAENGE VON 600 BYTES                      *
      *          NACH EINEM FIX-TEIL VON 36 BYTES FOLGT ENTSPR.      *
      *          DER AKTIONEN JEWEILS DER ZUGEHOERIGE SATZ           *
      *                                                              *
      *                                                              *
      *          STRUKTURERWEITERUNG SL= 590         26.03.88        *
      *          MODUS 'FA', 'FI' UND 'ON' AN                        *
      *          UU531S ANGEPASST                    06.12.89        *
      *                                                              *
      *          P2BFILLER AUF 12 BYTES VERKUERZT                    *
      *          P2BDDRKZ EINGEFUEHRT                01.08.90        *
      *                                                              *
      *          P2BEDKZ UND P2BFILLER2 VOR                          *
      *          PBABHSEG EINGEFUEGT / SEIPEL        20.09.91        *
      *                                                              *
      *          P2BDDRKZ1 EINGEBAUT, UM STRUKTUR                    *
      *          AN ABHÄNGIGE SEGMENTE ANZUPASSEN    28.11.91        *
      *                                                              *
      *AFOP0013  SATZLÄNGE UM 30 BYTE AUF 630 BYTE VERGROESSERT      *
      *          REDEFINE  P2AABHSEG AUF PBDATEN FÜR AVBG            *
      *          EINGEFÜHRT          / BOHLINGER     13.06.94        *
      *                                                              *
      *AFRZ0013  UMSTELLUNG KUS / RACF                               *
      *          EINGEFÜHRT          / WIECHERS      26.08.94        *
      *AFRZ0023  DEF PBUSERID GEÄNDERT               ......95        *
      *                                                              *
      ****************************************************************
       01  PBSATZ.
           02 PBVORSATZ.
              05 PBMODUS                      PIC XX.
              05 PBDATUM                      PIC 9(7)        COMP-3.
              05 PBUHR                        PIC 9(9)        COMP-3.
              05 PBTERM                       PIC X(4).
260894*       05 PBPERSNR                     PIC 9(7).
              05 PBUSERID                     PIC X(7).
           02 PBZUNAME                        PIC X(14).
           02 PBDATEN                         PIC X(594).
      ****************************************************************
      *                                                              *
      *    1. ROOT-SEGMENT / BEI DEN MODI 'RA', 'RN', 'RR'           *
      *                                                              *
      ****************************************************************
           02 PBROOT                          REDEFINES PBDATEN.
              03 P1BSA                        PIC 99.
              03 P1BKEY.
                 05  P1BGES                   PIC 99.
                 05  P1BVSNR                  PIC 9(15).
              03 P1BINTVSNR                   PIC 9(15).
              03 P1BSATZ.
                 05  P1BEKRIT.
                     10  P1BEPGM              PIC X(8).
                     10  P1BEDATUM            PIC 9(7)        COMP-3.
                     10  P1BEZEIT             PIC 9(7)        COMP-3.
                     10  P1BPSNR              PIC 9(7)        COMP-3.
                 05  P1BAVERM                 PIC 9(7)        COMP-3.
                 05  P1BAVERMPRZ              PIC X.
                 05  P1BVVERM                 PIC 9(7)        COMP-3.
                 05  P1BVVERMPRZ              PIC X.
                 05  P1BKDBETRGV              PIC 9(7)        COMP-3.
                 05  P1BFILLER                PIC X(22).
                 05  P1BKDBETRANZ             PIC 99.
                 05  P1BTABBETR               OCCURS 12.
                     10  P1BKDBETR            PIC 9(7)        COMP-3.
                     10  P1BGUELTAB           PIC 9(7)        COMP-3.
                     10  P1BAENDDAT           PIC 9(7)        COMP-3.
      ****************************************************************
      *                                                              *
      *          ABH.-SEGMENT   BEI DEN MODI 'IA', 'IG' UND 'IN'     *
      *          >> D V A G  <<                                      *
      ****************************************************************
           02 PBABHSEG                        REDEFINES PBDATEN.
              03 P2BSA                        PIC XX.
              03 P2BKEY.
                 05  P2BGES                   PIC XX.
                 05  P2BINTVSNR               PIC 9(15).
                 05  P2BKFAELLIG              PIC 9(6).
                 05  P2BPRODSCHL              PIC 99.
                 05  P2BLFDNR                 PIC 99.
              03 P2BSATZ.
                 05  P2BEKRIT.
                     10  P2BEPGM              PIC X(8).
                     10  P2BEDATUM            PIC 9(7)        COMP-3.
                     10  P2BEZEIT             PIC 9(7)        COMP-3.
                     10  P2BPSNR              PIC 9(7)        COMP-3.
                 05  P2BFREIKZ                PIC X.
                 05  P2BVSNR                  PIC 9(15)       COMP-3.
                 05  P2BFAELLIG               PIC 9(7)        COMP-3.
                 05  P2BBUCHDATPART           PIC 9(7)        COMP-3.
                 05  P2BBUCHDATAML            PIC 9(7)        COMP-3.
                 05  P2BTERMIN                PIC 9(7)        COMP-3.
                 05  P2BKDBETR-AVERM          PIC 9(7)        COMP-3.
                 05  P2BDDRKZ                 PIC X(01).
                 05  P2BFILLER                PIC X(12).
                 05  P2BROLAND                REDEFINES P2BFILLER.
                     10  P2BROLFILL           PIC X(3).
                     10  P2BIPBETR            PIC 9(7)V99     COMP-3.
                     10  P2BIPPROVS           PIC 99V9        COMP-3.
                     10  P2BLAUFZEIT          PIC XX.
                 05  P2BLFDNRE                PIC 99.
                 05  P2BPRODART               PIC 9.
                 05  P2BPROVPFLSU             PIC S9(7)V99    COMP-3.
                 05  P2BPRORATASU             PIC S9(7)V99    COMP-3.
                 05  P2BPROVSATZ              PIC S999V9      COMP-3.
                 05  P2BGESPROVBETR           PIC S9(7)V99    COMP-3.
                 05  P2BEDKZ                  PIC X.
                 05  P2BDDRKZ1                PIC X(01).
                 05  P2BFILLER2               PIC X(8).
                 05  P2BSTRUKANZ              PIC 99.
                 05  P2BSTRUKTABELLE.
                     07  P2BSTRUKTAB          OCCURS 20.
                         10  P2BVBNR          PIC 9(7)        COMP-3.
                         10  P2BSTRUKSTU      PIC 999.
                         10  P2BDIFFPROV      PIC S999V9      COMP-3.
                         10  P2BPROVBETR      PIC S9(5)V99    COMP-3.
                         10  P2BPROVRUECK     PIC S9(5)V99    COMP-3.
                         10  P2BPROVEINH      PIC S9(6)V9     COMP-3.
      ****************************************************************
      *                                                              *
      *          ABH.-SEGMENT   BEI DEN MODI 'IA', 'IG' UND 'IN'     *
      *          >> A V B G  <<                                      *
      ****************************************************************
940613     02 PBABHSEG-AVBG                   REDEFINES PBDATEN.
940613        03 P2ASA                        PIC XX.
940613        03 P2AKEY.
940613           05  P2AGES                   PIC XX.
940613           05  P2AINTVSNR               PIC 9(15).
940613           05  P2AKFAELLIG              PIC 9(6).
940613           05  P2APRODSCHL              PIC 99.
940613           05  P2ALFDNR                 PIC 99.
940613        03 P2ASATZ.
940613           05  P2AEKRIT.
940613               10  P2AEPGM              PIC X(8).
940613               10  P2AEDATUM            PIC 9(7)        COMP-3.
940613               10  P2AEZEIT             PIC 9(7)        COMP-3.
940613               10  P2APSNR              PIC 9(7)        COMP-3.
940613           05  P2AFREIKZ                PIC X.
940613           05  P2AVSNR                  PIC 9(15)       COMP-3.
940613           05  P2AFAELLIG               PIC 9(7)        COMP-3.
940613           05  P2ABUCHDATPART           PIC 9(7)        COMP-3.
940613           05  P2ABUCHDATAML            PIC 9(7)        COMP-3.
940613           05  P2ATERMIN                PIC 9(7)        COMP-3.
940613           05  P2AKDBETR-AVERM          PIC 9(07)       COMP-3.
940613           05  P2ADDRKZ                 PIC X(01).
940613           05  P2AFILLER                PIC X(12).
940613           05  P2AAVBG                  REDEFINES P2AFILLER.
940613               10  P2AHAFTZEIT-BEGINN   PIC 9(06) PACKED-DECIMAL. .
940613               10  P2AHAFTZEIT-ENDE     PIC 9(06) PACKED-DECIMAL.
940613               10  P2AAVBG-REST         PIC X(04).
940613           05  P2ALFDNRE                PIC 99.
940613           05  P2APRODART               PIC 9.
940613           05  P2APROVPFLSU             PIC S9(7)V99    COMP-3.
940613           05  P2APRORATASU             PIC S9(7)V99    COMP-3.
940613           05  P2APROVSATZ              PIC S999V9      COMP-3.
940613           05  P2AGESPROVBETR           PIC S9(7)V99    COMP-3.
940613           05  P2AEDKZ                  PIC X.
940613           05  P2ADDRKZ1                PIC X.
940613           05  P2AFILLER2               PIC X(8).
940613           05  P2AFILLER3               PIC X(30).
940613           05  P2A-ALLIANZ    REDEFINES P2AFILLER3.
940613               10  P2AJNBEITR         PIC 9(07)V99 PACKED-DECIMAL.
940613               10  P2AINKSTD            PIC 9(06) PACKED-DECIMAL.
940613               10  P2ATARIF             PIC X(04).
940613               10  P2ABESTSCHL          PIC X(02).
940613               10  P2AALLIANZ-REST      PIC X(15).
940613           05  P2ASTRUKANZ              PIC 99.
940613           05  P2ASTRUKTABELLE.
940613               07  P2ASTRUKTAB          OCCURS 20.
940613                   10  P2AVBNR          PIC 9(7)        COMP-3.
940613                   10  P2ASTRUKSTU      PIC 999.
940613                   10  P2ADIFFPROV      PIC S999V9      COMP-3.
940613                   10  P2APROVBETR      PIC S9(5)V99    COMP-3.
940613                   10  P2APROVRUECK     PIC S9(5)V99    COMP-3.
940613                   10  P2APROVEINH      PIC S9(6)V9     COMP-3.

      ****************************************************************
      *                                                              *
      *          FEHLER-SATZ   BEI DEN MODI 'FA' 'FI' UND 'ON'       *
      *                                                              *
      ****************************************************************
           02  PBFEHL                         REDEFINES PBDATEN.
               03 PBKEY.
                  05 PBGES                    PIC 99.
                  05 PBVSNR                   PIC 9(15).
                  05 PBFAELLIG                PIC 9(6).
                  05 PBPRODSCHL               PIC 99.
                  05 PBPRODART                PIC 9.
                  05 PBLFDNR                  PIC 99.
               03 PBERSTELL                   PIC 9(13).
               03 PBVBNR                      PIC 9(6).
               03 PBNAME                      PIC X(20).
               03 PBABRECHDAT                 PIC 9(6).
               03 PBBUCHDATPART               PIC 9(6).
               03 PBBVRD                      PIC XX.
               03 PBINTVSNR                   PIC 9(7).
               03 PBRESERVE                   PIC X(29).
               03 PBFEHLERCODE                PIC 99.
               03 PBPRODEIN.
                  05  PBPROVPFLSU             PIC 9(7)V99.
                  05  PBPROVSATZ              PIC 99V9.
                  05  PBPRORATASU             PIC 9(7)V99.
      ****************************************************************
      *                                                              *
      *     PRODUKTEN-SATZ   BEI DEN MODI 'PW' 'PD' 'PA' UND 'PN'    *
      *                                                              *
      ****************************************************************
           02  PBPROD                         REDEFINES PBDATEN.
               03  PBTSA                        PIC XX.
               03  PBTKEY.
                   05  PBTGES                   PIC 99.
                   05  PBTPRODSCHL              PIC 99.
                   05  PBTBEGINN                PIC 9(6).
                   05  PBTABRECHDAT             PIC 9(6).
                   05  PBTFAELLIG               PIC 9(6).
                   05  PBTRESDAT                PIC 9(6).
               03  PBTDATEN.
                   05  PBTGRUND                 PIC 9(4)V99.
                   05  PBTRUECK                 PIC 99.
                   05  PBTVERT                  PIC 9(4)V99.
                   05  PBTCOVER                 PIC 9(4)V99.
                   05  PBTPROVPFL               PIC 9(7)V99.

