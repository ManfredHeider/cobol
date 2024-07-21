      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1481.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1481.CPv  $
      *
      *   Rev 2.3   Nov 03 2006 13:30:58   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   30 Mar 2001 09:52:18   T13EX22
      *APA00131: Feldbezeichnungen standardisiert
      *
      *   Rev 2.1   30 Mar 2001 07:13:04   T13EX22
      *APA00131: PEG-BfG-Bank
      *
      *   Rev 2.0   09 Jan 2001 10:22:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1481  PEG-DATENSATZ VON BFG-BANK (66)     *
      *    ---------------------------------------------------------  *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!28.02.01!2.1 ! NEUERSTELLUNG             ! MUND     *
      * APA00452!03.11.06!2.2 ! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
             15 BFG-BANK-DATENSATZ.
                20  BAND.
                    25   ORGA.
                     30  GES                PIC 99.
                     30  ORGGS              PIC S9(3)      COMP-3.
                     30  VERM               PIC S9(10)     COMP-3.
                     30  PIF.
                      35 PIF-FILLER1        PIC XX.
                      35 PIF-AVERM.
                       40 PIF-AVERM1-6      PIC 9(6).
                       40 PIF-AVERM7-8      PIC 99.
                      35 PIF-REST           PIC X(10).
      *
                    25   PRODUKT.
                     30  BFGGES             PIC 9.
                     30  VNR.
                      35 VNR-NL              PIC S9(4)      COMP-3.
                      35 VNR-KONTO           PIC S9(10)     COMP-3.
                     30  FONDS              PIC S9(3)      COMP-3.
031106*              30  PRODSCHL           PIC 99.
031106               30  TARIF              PIC 99.
                     30  NAME               PIC X(30).
                     30  STRASSE            PIC X(30).
                     30  PLZORT             PIC X(30).
                     30  ANTRAG-DAT         PIC S9(8)      COMP-3.
                     30  FAELLIGKEIT-DAT    PIC S9(8)      COMP-3.
                     30  KUSCHUABL-DAT      PIC S9(8)      COMP-3.
      *
                    25   PROVISIONFELDER.
                     30  PRODSUM            PIC S9(9)V99   COMP-3.
                     30  PG-PPFL-SUMME           PIC S9(9)V99   COMP-3.
                     30  PG-GSMT-PROV        PIC S9(7)V99   COMP-3.
                     30  UMSATZART          PIC X.
                     30  UMSATZKENN         PIC X.
                     30  PG-PROV-STZ           PIC S9(3)V99   COMP-3.
      *
                    25   KENNZEICHEN.
                     30  NOTSELBST          PIC X.
      *
                    25   WETTBEWERB.
                     30  WETTBEWZAHL        PIC S9(3)      COMP-3.
      *
                    25   RESERVE-C.
990205               30  C-WAEHRUNG         PIC X(03).
990205               30  FILLER             PIC X(52).
      *
                    25   RESERVE-C-INVEST   REDEFINES     RESERVE-C.
                     30  DVDATUM            PIC S9(8)      COMP-3.
                     30  UMSATZSORT         PIC 99.
990205               30  FILLER             PIC X(48).
      *
      ******** ENDE COPY-MEMBER **************************************
