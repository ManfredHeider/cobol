      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU533S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/UU533S.CPv  $
      *
      *   Rev 2.1   Nov 02 2006 14:57:08   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Feb 27 1999 15:15:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:01:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    07.06.94 14:33:33
      ****************************************************************
      *                                                              *
      *          DATEI FUER DIE SATZART 815  (SA815)                 *
      *          FESTE LAENGE; LAENGE =  135 BYTES                   *
      *          SATZART   : 815                                     *
      *          COPYMEMBER: UU533S                                  *
      *                                                              *
      *                                                              *
      * VERS:    02  VOM 12.02.88      GEÄNDERT SCHMOLL              *
      *  - 15 STELLIGE VSNR FUER BFG                                 *
      * VERS:    03  VOM 06.06.94      GEÄNDERT BOHLINGER            *
      *  - TARKZ UND BEWART IN PIC X GEÄNDERT                        *
      *  - JNBEITRAG MITTELS REDEFINE EINGESTELLT                    *
      * VERS:  2.1   VOM 01.11.2006             SCHEIDEL             *
      *  - DREISTELLIGE PRODUKTSCHLUESSEL                            *
      *                                                              *
      *                                                              *
      ****************************************************************
       01  UU533S.
           05  UU533S-SA                       PIC 999.
           05  UU533S-GES                      PIC 99.
           05  UU533S-VERSNR.
            10 UU533S-VSNR                     PIC 9(11).
            10 UU533S-FILLER                   PIC 9.
           05  UU533S-VERSNRX      REDEFINES   UU533S-VERSNR.
            10 UU533S-VSFILLER                 PIC 9(7)       COMP-3.
            10 UU533S-VSNRX                    PIC 9(15)      COMP-3.
           05  UU533S-NAME                     PIC X(14).
           05  UU533S-FAELLIG                  PIC 9(6).
           05  UU533S-DAUER                    PIC 99.
940606*    05  UU533S-TARKZ                    PIC 9(4).
940606     05  UU533S-TARKZ                    PIC X(4).
           05  UU533S-BEW.
               10  UU533S-BEWSCHL              PIC 9(4).
               10  UU533S-BEWBT                PIC 9(4).
940606     05  UU533S-FILLER2                  PIC X(6).
940606     05  UU533S-LV-KZ REDEFINES UU533S-FILLER2.
               10  UU533S-STORRES-STEU         PIC X.
               10  UU533S-RESERVE              PIC X.
               10  UU533S-LEISTUNGSKZ          PIC X.
               10  UU533S-LV-ZAHLW             PIC 9.
               10  UU533S-DYNFKZ1              PIC 9.
               10  UU533S-DYNFKZ2              PIC 9.
940606     05  UU533S-BEITRAG REDEFINES UU533S-FILLER2.
940606         10  UU533S-JNBEITR              PIC 9(7)V99 COMP-3.
940606         10  UU533S-BEITRAG-REST         PIC 9.
940606*    05  UU533S-BEWART                   PIC 99.
940606     05  UU533S-BEWART                   PIC XX.
           05  UU533S-BWERTSU                  PIC S9(7)    COMP-3.
           05  UU533S-BWERTBTG                 PIC S9(5)V99 COMP-3.
           05  UU533S-PROV                     PIC 9(8).
           05  UU533S-KTOAP.
               10  UU533S-KTOAP1               PIC 9(7).
               10  UU533S-KTOAP2               PIC 99.
           05  UU533S-DIFFPROV                 PIC S999V99  COMP-3.
           05  UU533S-GRSATZ                   PIC S999     COMP-3.
           05  UU533S-VGART                    PIC 9.
011106*    05  UU533S-PRODSCHL                 PIC 99.
011106     05  UU533S-PROD-PROD-SCHL           PIC S9(004) BINARY.
           05  UU533S-PRODART                  PIC 9.
           05  UU533S-NOTE                     PIC XXX.
           05  UU533S-PROVPFLSU                PIC S9(7)V99 COMP-3.
           05  UU533S-PRORATASU                PIC S9(7)V99 COMP-3.
           05  UU533S-AVERM.
               10  UU533S-AVERM1               PIC 9(7).
               10  UU533S-AVERM2               PIC 99.
           05  UU533S-PROVAUSZ                 PIC S9(7)V99 COMP-3.
           05  UU533S-PROVRUEC                 PIC S9(7)V99 COMP-3.
           05  UU533S-PROVEINH                 PIC S9(6)V9  COMP-3.
           05  UU533S-BUCHDATUM.
               10  UU533S-BUCHDAT-TT           PIC 99.
               10  UU533S-BUCHDAT-MM           PIC 99.
               10  UU533S-BUCHDAT-JJ           PIC 99.

