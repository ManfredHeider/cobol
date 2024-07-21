      *  -------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drop0005.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drop0005.cpv  $
      *
      *   Rev 2.1   Jul 12 1999 15:17:26   T13EX15
      *AOP00183: Einf. Einheitenaufschlag (Ges.25)
      *
      *   Rev 2.0   Feb 27 1999 14:14:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   17 Dec 1998 08:35:42   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.1   Mar 05 1996 11:04:04   T13AE16
      *AFOP0093 Ges. 95 neu
      *
      *   Rev 1.0   Nov 04 1995 08:57:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    30.03.94 14:02:17
      ****************************************************************
      *                                                              *
      *   DROP0005 - COPYMEMBER                                      *
      *   ========                                                   *
      *                                                              *
      *          BEWEGUNGS-DATEI ABHAENGIGES SEGMENT  AVBG WIEN      *
      *          VARIABELE LAENGE, MINIMAL 198, MAXIMAL 594 BYTES    *
      *          SCHLUESSEL: P2KEY                                   *
      *          ALT.-IND.:  P1INTVR                                 *
      *                                                              *
      * AUFTAG   ! GRUND                !VER ! DATUM    ! NAME       *
      ****************************************************************
      * AFOP0013 ! EINFÜHRUNG AVBG WIEN ! 01 ! 30.03.94 ! BOHLINGER  *
      * AFOP0093 ! EINFÜHRUNG AVBG      !    !          !            *
      *          ! Sonderges. 95        ! .. ! ......95 ! BOHLINGER  *
      * AOP00183 ! EINFÜHRUNG Kennz. für!    !          !            *
      *          ! Einh.-Aufschl. (G 25)! .. ! 12.07.99 ! Gsellhofer *
      *          !                      !    !          !            *
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
940330        05  P2AVBG                REDEFINES P2FILLER.
940330            10  P2AVBG-REST           PIC X(04).
940330            10  P2HAFTZEIT-BEGINN     PIC 9(06) PACKED-DECIMAL.
940330            10  P2HAFTZEIT-ENDE       PIC 9(06) PACKED-DECIMAL.
940330            10  P2AVBG-REST2          PIC X(05).
g95           05  P2GES91               REDEFINES P2FILLER.
g95               10  P2GES91-GRUNDPROV PIC 9(02)V999.
g95               10  P2GES91-REST      PIC X(12).
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
              05  P2FILLER3             PIC X(30).
              05  P2ALLIANZ             REDEFINES P2FILLER3.
                  10  P2JNBEITR         PIC 9(07)V99 PACKED-DECIMAL.
940330            10  P2INKSTD          PIC 9(06)    PACKED-DECIMAL.
940330            10  P2TARIF           PIC X(04).
940330            10  P2BESTSCHL        PIC X(02).
990712            10  P2GES25-EH-AUFSCHLAG   PIC X(01).
990712            10  P2ALLIANZ-REST    PIC X(14).
990712*           10  P2ALLIANZ-REST    PIC X(15).
              05  P2STRUKANZ            PIC 99.
              05  P2STRUKTABELLE.
                  07  P2STRUKTAB OCCURS 2 TO 20 DEPENDING ON P2STRUKANZ.
                      10  P2VBNR        PIC 9(7)        COMP-3.
                      10  P2STRUKSTU    PIC 999.
                      10  P2DIFFPROV    PIC S999V9      COMP-3.
                      10  P2PROVBETR    PIC S9(5)V99    COMP-3.
                      10  P2PROVRUECK   PIC S9(5)V99    COMP-3.
                      10  P2PROVEINH    PIC S9(6)V9     COMP-3.

