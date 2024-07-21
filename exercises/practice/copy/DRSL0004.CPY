      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0004.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRSL0004.CPv  $
      *
      *   Rev 2.3   05 Dec 2001 09:48:44   t13ae13
      *AVB00544: Umstellung 40%-Klausel auf variabel
      *
      *   Rev 2.2   30 Mar 2001 07:01:22   T13AE13
      *ASL00068 - ORG für GS-D analog ORG für RGS
      *
      *   Rev 2.1   Mar 16 2000 16:45:20   T13AE35
      *ASL00047- neu, Copy fuer SL000600
      *ORG-Datei fuer RGS, die nur Umsatz-
      *bedingungen erfuellen
      *ab Auftrag ASL00068 ORG-Datei für alle VB's, die nur Umsatz-
      *bedigungen erfüllen (Nichterfüller)
      *
      *   Rev 2.0   Feb 11 2000 15:19:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00047!02.03.00!01.0! Neuerstellung             ! Walb     *
      * ASL00068!23.02.01!02.0! neue Felder               ! Walb     *
      * AVB00544!04.12.01!2.3 ! '-40-' jetzt variabel     !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *****       (COPY-MEMBER für ORG-RGS-Datei)                     *
      *            COPY-MEMBER für ORG-NEF-Datei                      *
      *            DATEINUMMER: SL000600 KSDS                         *
      *            COPYMEMBER:  DRSL0004 (analog UU269S)              *
      *            SATZLAENGE:  280 BYTES                             *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0004.
           05  DRSL0004-REC.
               10  DRSL0004-KEY.
                   15  DRSL0004-VBNR         PIC 9(07).
                   15  DRSL0004-QUARTAL      PIC 9(01).
               10  DRSL0004-BESTANDSKZ       PIC 9(01).
               10  DRSL0004-DIR              PIC 9(03).
               10  DRSL0004-VB-STUFE         PIC X(04).
               10  DRSL0004-NAME             PIC X(20).
               10  DRSL0004-EQF              PIC 9(03)V99 COMP-3.
               10  DRSL0004-EQF-KORR1        PIC 9(03)V99 COMP-3.
               10  DRSL0004-EQF-KORR2        PIC 9(03)V99 COMP-3.
               10  DRSL0004-EQF-KORR3        PIC 9(03)V99 COMP-3.
               10  DRSL0004-VB-NETGRUP       PIC S9(09)V9 COMP-3.
               10  DRSL0004-VB-NETGRUP-KORR1 PIC S9(09)V9 COMP-3.
               10  DRSL0004-VB-NETGRUP-KORR2 PIC S9(09)V9 COMP-3.
               10  DRSL0004-VB-NETGRUP-KORR3 PIC S9(09)V9 COMP-3.
041201*        10  DRSL0004-40-UMSATZ        PIC S9(09)V9 COMP-3.
041201*        10  DRSL0004-40-ANZAHL        PIC 9(01).
041201*        10  DRSL0004-40-UNTERST       OCCURS 4.
041201*            15  DRSL0004-40-VBNR      PIC 9(07).
041201*            15  DRSL0004-40-NETGES    PIC S9(09)V9 COMP-3.
041201*            15  DRSL0004-40-ABZUG     PIC S9(08)V9 COMP-3.
041201         10  DRSL0004-ABZ-UMSATZ       PIC S9(09)V9 COMP-3.
041201         10  DRSL0004-ABZ-ANZAHL       PIC 9(01).
041201         10  DRSL0004-ABZ-UNTERST      OCCURS 4.
041201             15  DRSL0004-ABZ-VBNR     PIC 9(07).
041201             15  DRSL0004-ABZ-NETGES   PIC S9(09)V9 COMP-3.
041201             15  DRSL0004-ABZ-ABZUG    PIC S9(08)V9 COMP-3.
               10  DRSL0004-EQF-ABZUG        PIC S9(07)   COMP-3.
               10  DRSL0004-ORG-ZUSCHUSS-TAB OCCURS 3.
                   15  DRSL0004-ORG-ZUSCHUSS PIC S9(05)   COMP-3.
                   15  DRSL0004-ORG-KZ       PIC  X(01).
               10  DRSL0004-LEI-ZUSCHUSS-TABELLE.
                   15  DRSL0004-LEI-ZUSCHUSS-TAB OCCURS 3.
                       20  DRSL0004-LEI-ZUSCHUSS PIC S9(05) COMP-3.
                       20  DRSL0004-LEI-KZ       PIC  X(01).
               10  DRSL0004-LEI-DATEN.
                   15  DRSL0004-D7-STUFE     PIC  9(01).
                   15  DRSL0004-LEI-UMSATZ   PIC S9(09)V9 COMP-3.
               10  DRSL0004-AUSDAT           PIC  9(07)   COMP-3.
               10  DRSL0004-UMSTUFDAT        PIC  9(07)   COMP-3.
               10  FILLER                    PIC  X(22).
               10  DRSL0004-EINH-12-MON      PIC S9(09)V9 COMP-3.
230201*        10  FILLER                    PIC  X(58).
230201         10  DRSL0004-KTY-GES          PIC S9(04)   COMP.
230201         10  DRSL0004-KNR-GES          PIC S9(04)   COMP.
230201         10  DRSL0004-KTY-WAEHR        PIC S9(04)   COMP.
230201         10  DRSL0004-KNR-WAEHR        PIC S9(04)   COMP.
230201         10  FILLER                    PIC  X(50).
           05  DRSL0004-DUMMY REDEFINES DRSL0004-REC.
               10  DRSL0004-D-KEY.
                   15  DRSL0004-D-VBNR       PIC 9(07).
                   15  DRSL0004-D-QUARTAL    PIC 9(01).
               10  DRSL0004-D-BESTANDSKZ     PIC 9(01).
               10  DRSL0004-D-DIR            PIC 9(03).
               10  DRSL0004-JAHR             PIC 9(02).
               10  DRSL0004-ERM-QUARTAL      PIC 9(01).
               10  DRSL0004-ORG-ERM-DAT      PIC 9(07)   COMP-3.
               10  DRSL0004-LEI-ERM-DAT      PIC 9(07)   COMP-3.
               10  DRSL0004-VB2-DAT          PIC 9(07)   COMP-3.
               10  FILLER                    PIC X(253).
      *---------------------------------------------------------------*
