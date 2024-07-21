      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU269S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/UU269S.CPv  $
      *
      *   Rev 2.2   05 Dec 2001 09:45:38   t13ae13
      *AVB00544: Umstellung 40%-Klausel auf variabel
      *
      *   Rev 2.1   21 Dec 2000 11:18:20   T13AE27
      *AVBG-Anpassung
      *
      *   Rev 2.0   Feb 27 1999 15:15:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:59:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   6    06.10.95 15:10:58
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : ORG-ZUSCHUSS                  *
      *  DATEINUMMER       : UU0269                        *
      *  COPYMEMBER        : UU269S                        *
      *  SATZLAENGE        : 280 BYTES                     *
      *  SCHLUESSEL        : UU269S-VBNR                   *
      *  SCHLUESSELLAENGE  :   8 BYTES                     *
      *  ERSTELLDATUM      : 29.01.1991                    *
      *                                                    *
      *  VERS    02 AB 07.03.91:                           *
      *    NEUE FELDER: UU269S-QUARTAL                     *
      *                 UU269S-ORG-AB                      *
      *    ==> NEUER SCHLUESSEL: UU269S-VBNR/UU269S-QUARTAL*
      *    ==> NEUE  SATZLAENGE: 285 BYTE                  *
      *                                                    *
      *  VERS    03 AB 12.03.91:                           *
      *    GELOESCHTES FELD:                               *
      *                 UU269S-ORG-AB                      *
      *    ==> SCHLUESSELLAENGE:   8 BYTES                 *
      *    ==> NEUE  SATZLAENGE: 280 BYTES                 *
      *    ==> FILLER SATZENDE :   8 BYTES                 *
      *                                                    *
      *  VERS    04 AB 13.05.91:                           *
      *    FILLER ERSETZT DURCH:                           *
      *    ==> UU269S-AUSDAT    PIC 9(7) COMP-3            *
      *        UU269S-UMSTUFDAT PIC 9(7) COMP-3            *
      *    ==> SATZLAENGE BLEIBT BESTEHEN = 280 BYTE       *
      *                                                    *
      *  VERSION 05 AB 21.01.92:                           *
      *    ==> UU269S-VBNR-P           ENTFAELLT           *
      *    ==> UU269S-MTL-EINH         ENTFAELLT           *
      *    ==> UU269S-MTL-UMSATZ       ENTFAELLT           *
      *    ==> UU269S-DATUM            ENTFAELLT           *
      *    ==> UU269S-NET-ORG-ZUSCHUSS ENTFAELLT           *
      *    FOLGENDE FELDER SIND NEU:                       *
      *    ==> UU269S-ORG-KZ         PIC  X(01)            *
      *    ==> UU269S-LEI-ZUSCHUSS   PIC S9(05) COMP-3     *
      *    ==> UU269S-LEI-KZ         PIC  X(01)            *
      *    ==> UU269S-D7-STUFE       PIC  9(01)            *
      *    ==> UU269S-LEI-UMSATZ     PIC S9(09)V9 COMP-3   *
      *    ==> ALLE BETRAG- UND EINHEIT-FELDER SIND        *
      *        GEPACKT.                                    *
      *    ==> SATZLAENGE BLEIBT BESTEHEN = 280 BYTE       *
      *                                                    *
      *  VERSION 06 AB LAUF 10/1994 (AUFTRAG AFVB0132 -    *
      *  BERGHOFER):                                       *
      *  STATUS-A-VB UND STATUS-A-PRAXIS BEINHALTET, OB    *
      *  STATUS-A AM STICHTAG BESTAND                      *
      *  (AM ANFANG DES QUARTALS)                          *
      *    ==> UU269S-STATUS-TABELLE                       *
      *    ==>    UU269S-STATUS-A-VB      PIC X(1).        *
      *    ==>    UU269S-ST-A-VB-VON      PIC X(10).       *
      *    ==>    UU269S-STATUS-A-PRAXIS  PIC X(1).        *
      *    ==>    UU269S-ST-A-PR-VON      PIC X(10).       *
      *   FILLER VERKLEINERT AUF 64 BYTE                   *
      *    ==> SATZLAENGE BLEIBT BESTEHEN = 280 BYTE       *
      *                                                    *
      *  VERSION 07 AB LAUF 10/1995 (AUFTRAG AFVB0237 -    *
      *  SCHÖLLHAMMER):                                    *
      *  STATUS-A-FELDER (AUS VERSION 06) WIEDER RAUS      *
      *   GENOMMEN UND DURCH FILLER ERSETZT.               *
      *  NEU:                                              *
      *   UU269S-EINH-12-MON BEINHALTET DIE EINHEITEN      *
      *   NETTO-NEU-EIGENGESCHÄFT (VOR DEM QUARTAL)        *
      *    ==> UU269S-EINH-12-MON       PIC S9(9)V9 COMP-3 *
      *   FILLER VERKLEINERT AUF 58 BYTE                   *
      *    ==> SATZLAENGE BLEIBT BESTEHEN = 280 BYTE       *
      *  NEU (02.11.00): wg. Anpassungen an AVBG:          *
      *      Gesellschaft und Währung eingefügt            *
      *                                                    *
      * AUFTRAG ! DATUM ! VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00544!04.12.01! 2.1! '40-' jetzt variabel      ! Walb     *
      *                                                              *
      ****************************************************************

       01  UU269S.
           05  UU269S-REC.
               10  UU269S-KEY.
                   15  UU269S-VBNR         PIC 9(07).
                   15  UU269S-QUARTAL      PIC 9(01).
               10  UU269S-BESTANDSKZ       PIC 9(01).
               10  UU269S-DIR              PIC 9(03).
               10  UU269S-VB-STUFE         PIC X(04).
               10  UU269S-NAME             PIC X(20).
               10  UU269S-EQF              PIC 9(03)V99 COMP-3.
               10  UU269S-EQF-KORR1        PIC 9(03)V99 COMP-3.
               10  UU269S-EQF-KORR2        PIC 9(03)V99 COMP-3.
               10  UU269S-EQF-KORR3        PIC 9(03)V99 COMP-3.
               10  UU269S-VB-NETGRUP       PIC S9(09)V9 COMP-3.
               10  UU269S-VB-NETGRUP-KORR1 PIC S9(09)V9 COMP-3.
               10  UU269S-VB-NETGRUP-KORR2 PIC S9(09)V9 COMP-3.
               10  UU269S-VB-NETGRUP-KORR3 PIC S9(09)V9 COMP-3.
041201*        10  UU269S-40-UMSATZ        PIC S9(09)V9 COMP-3.
041201*        10  UU269S-40-ANZAHL        PIC 9(01).
041201*        10  UU269S-40-UNTERST       OCCURS 4.
041201*            15  UU269S-40-VBNR      PIC 9(07).
041201*            15  UU269S-40-NETGES    PIC S9(09)V9 COMP-3.
041201*            15  UU269S-40-ABZUG     PIC S9(08)V9 COMP-3.
041201         10  UU269S-ABZ-UMSATZ       PIC S9(09)V9 COMP-3.
041201         10  UU269S-ABZ-ANZAHL       PIC 9(01).
041201         10  UU269S-ABZ-UNTERST      OCCURS 4.
041201             15  UU269S-ABZ-VBNR     PIC 9(07).
041201             15  UU269S-ABZ-NETGES   PIC S9(09)V9 COMP-3.
041201             15  UU269S-ABZ-ABZUG    PIC S9(08)V9 COMP-3.
               10  UU269S-EQF-ABZUG        PIC S9(07)   COMP-3.
               10  UU269S-ORG-ZUSCHUSS-TAB OCCURS 3.
                   15  UU269S-ORG-ZUSCHUSS PIC S9(05)   COMP-3.
                   15  UU269S-ORG-KZ       PIC  X(01).
               10  UU269S-LEI-ZUSCHUSS-TABELLE.
                   15  UU269S-LEI-ZUSCHUSS-TAB OCCURS 3.
                       20  UU269S-LEI-ZUSCHUSS PIC S9(05) COMP-3.
                       20  UU269S-LEI-KZ       PIC  X(01).
               10  UU269S-LEI-DATEN.
                   15  UU269S-D7-STUFE     PIC  9(01).
                   15  UU269S-LEI-UMSATZ   PIC S9(09)V9 COMP-3.
               10  UU269S-AUSDAT           PIC  9(07)   COMP-3.
               10  UU269S-UMSTUFDAT        PIC  9(07)   COMP-3.
110995*         10  UU269S-STATUS-TABELLE.
110995*             15   UU269S-STATUS-A-VB      PIC X(1).
110995*             15   UU269S-ST-A-VB-VON      PIC X(10).
110995*             15   UU269S-STATUS-A-PRAXIS  PIC X(1).
110995*             15   UU269S-ST-A-PR-VON      PIC X(10).
110995         10  FILLER                  PIC  X(22).
110995         10  UU269S-EINH-12-MON      PIC S9(09)V9 COMP-3.
110995*         10  FILLER                  PIC  X(64).
021100*        10  FILLER                  PIC  X(58).
021100         10  UU269S-KTY-GES          PIC  S9(4)   COMP.
021100         10  UU269S-KNR-GES          PIC  S9(4)   COMP.
021100         10  UU269S-KTY-WAEHR        PIC  S9(4)   COMP.
021100         10  UU269S-KNR-WAEHR        PIC  S9(4)   COMP.
021100         10  FILLER                  PIC  X(50).
           05  UU269S-DUMMY REDEFINES UU269S-REC.
               10  UU269S-D-KEY.
                   15  UU269S-D-VBNR       PIC 9(07).
                   15  UU269S-D-QUARTAL    PIC 9(01).
               10  UU269S-D-BESTANDSKZ     PIC 9(01).
               10  UU269S-D-DIR            PIC 9(03).
               10  UU269S-JAHR             PIC 9(02).
               10  UU269S-ERM-QUARTAL      PIC 9(01).
               10  UU269S-ORG-ERM-DAT      PIC 9(07)   COMP-3.
               10  UU269S-LEI-ERM-DAT      PIC 9(07)   COMP-3.
               10  UU269S-VB2-DAT          PIC 9(07)   COMP-3.
               10  FILLER                  PIC X(253).
