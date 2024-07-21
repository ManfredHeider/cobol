      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU520S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU520S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Dec 1998 08:40:42   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:01:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   7    04.09.92 10:06:01
      ****************************************************************
      *                                                              *
      *   UU520S - COPYMEMBER                                        *
      *   ======                                                     *
      *                                                              *
      *          BEWEGUNGS-DATEI ABHAENGIGES SEGMENT                 *
      *          VARIABELE LAENGE, MINIMAL 168, MAXIMAL 564 BYTES    *
      *          SCHLUESSEL: P2KEY                                   *
      *          ALT.-IND.:  P1INTVR                                 *
      *                                                              *
      *   VERS: 02       VOM 12. 02. 88      GEÄNDERT SCHMOLL        *
      *                                                              *
      *   VERS: 03       VOM 11. 10. 90      GEÄNDERT GEIGER         *
      *                                                              *
      *   VERS: 04       EINFUEHRUNG P2GES91-GRUNDPROV               *
      *                                                              *
      *   VERS: 05       VOM 20. 09. 91      GEÄNDERT SEIPEL         *
      *                                                              *
      *   VERS: 06       EINFÜHRUNG FELD P2DDRKZ / KRUMPHOLZ 20.12.91*
      *                                                              *
      *   VERS: 07       EINFÜHRUNG FELD P2AUSKZ / KRUMPHOLZ 13.07.92*
      *                                                              *
      ****************************************************************
       01  P2ABHSEG.
           03 P2SA                      PIC XX.
           03 P2KEY.
              05  P2GES                 PIC XX.
              05  P2INTVSNR             PIC 9(15).
              05  P2KFAELLIG            PIC 9(6).
              05  P2PRODSCHL            PIC 99.
              05  P2LFDNR               PIC 99.
           03 P2SATZ.
              05  P2EKRIT.
                  10  P2EPGM            PIC X(8).
                  10  P2EDATUM          PIC 9(7)        COMP-3.
                  10  P2EZEIT           PIC 9(7)        COMP-3.
                  10  P2PSNR            PIC 9(7)        COMP-3.
              05  P2FREIKZ              PIC X.
              05  P2VSNR                PIC 9(15)       COMP-3.
              05  P2FAELLIG             PIC 9(7)        COMP-3.
              05  P2BUCHDATPART         PIC 9(7)        COMP-3.
              05  P2BUCHDATAML          PIC 9(7)        COMP-3.
              05  P2TERMIN              PIC 9(7)        COMP-3.
              05  P2FILLER              PIC X(17).
              05  P2BAD                 REDEFINES P2FILLER.
                  10  P2BAD-LAUFZEIT    PIC 99.
                  10  P2BAD-REST        PIC X(15).
              05  P2BFG                 REDEFINES P2FILLER.
                  10  P2BFG-KDINH.
                   15 P2BFG-KDAVERM     PIC 9(8).
                  10  P2BFG-KDDAT       PIC 9(6).
                  10  P2BFG-REST        PIC X(3).
              05  P2ROLAND              REDEFINES P2FILLER.
                  10  P2ROLFILL         PIC X(8).
                  10  P2IPBETR          PIC 9(7)V99     COMP-3.
                  10  P2IPPROVS         PIC 99V9        COMP-3.
                  10  P2LAUFZEIT        PIC XX.
              05  P2GES91               REDEFINES P2FILLER.
                  10  P2GES91-GRUNDPROV PIC 9(02)V999.
                  10  P2GES91-REST      PIC X(12).
              05  P2LFDNRE              PIC 99.
              05  P2PRODART             PIC 9.
              05  P2PROVPFLSU           PIC S9(7)V99    COMP-3.
              05  P2PRORATASU           PIC S9(7)V99    COMP-3.
              05  P2PROVSATZ            PIC S999V9      COMP-3.
              05  P2GESPROVBETR         PIC S9(7)V99    COMP-3.
              05  P2EDKZ                PIC X.
              05  P2DDRKZ               PIC X.
              05  P2AUSKZ               PIC X.
              05  P2FILLER2             PIC X(7).
              05  P2STRUKANZ            PIC 99.
              05  P2STRUKTABELLE.
                  07  P2STRUKTAB OCCURS 2 TO 20 DEPENDING ON P2STRUKANZ.
                      10  P2VBNR        PIC 9(7)        COMP-3.
                      10  P2STRUKSTU    PIC 999.
                      10  P2DIFFPROV    PIC S999V9      COMP-3.
                      10  P2PROVBETR    PIC S9(5)V99    COMP-3.
                      10  P2PROVRUECK   PIC S9(5)V99    COMP-3.
                      10  P2PROVEINH    PIC S9(6)V9     COMP-3.

