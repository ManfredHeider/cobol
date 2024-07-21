      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DROP0026.CPY  $
      *    $REVISION:   2.2  $
      *
      *    $LOG:   L:/MF/CPY/DROP0026.CPV  $
      *
      *   REV 2.2   JAN 02 2007 15:14:40   T13EX44
      *APA00452: ERWEITERUNG DES PS AUF 3 STELLEN
      *
      *   REV 2.1   NOV 02 2006 14:48:50   T13EX44
      *APA00452: ERWEITERUNG DES PS AUF 3 STELLEN
      *
      *   REV 2.0   FEB 27 1999 14:14:48   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   17 DEC 1998 08:34:22   T13EX36
      *AOP00161 - JAHR2000-UMSTELLUNG
      *
      *   REV 1.0   31 JUL 1997 09:38:50   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *          VERKUERZTER OP-SATZ IN FFM                          *
      *          FESTE SATZLAENGE VON 140 STELLEN                    *
      *          SATZLÄNGE AUF 164 VERGÖSSERT (16. 5. 94)            *
      *          INDIZIERTE DATEI                                    *
      *                                                              *
      *   VERS: 02        VOM  12. 02. 88     GEÄNDERT  SCHMOLL      *
      *   VERS: 03        VOM  24. 04. 90     GEÄNDERT  KISKO        *
      *   - AUFTEILEN UU531S-ERSTELL:                                *
      *          UU531S-ERSTELLDATPA  = ERSTELL-DATUM-PARTNER        *
      *          UU531S-ERSTELLDATOP  = ERSTELL-DATUM-OPAL-ÜBERNAHME *
      *   VERS: 04        VOM  30.08.90       GEÄNDERT  GEIGER       *
      *   - EINFUEHRUNG DDR-KENNZEICHEN FUER AMV UND ROLAND          *
      *   VERS: 05        VOM  30.08.90       GEÄNDERT  GEIGER       *
      *   - EINFUEHRUNG GRUNDPROVISIONSSATZ FUER GES 91              *
      *   VERS: 06        VOM  28.09.90       GEÄNDERT  GEIGER       *
      *   - EINFUEHRUNG DDR-KENNZEICHEN FUER BADENIA                 *
      *   VERS: 07        VOM  05.02.91       GEÄNDERT  HECKER       *
      *   - EINFUEHRUNG UNFALL-VERS-KENNZEICHEN UU531S-UNVKZ         *
      *   VERS: 08        VOM  03.06.91       GEÄNDERT  SCHMIDT      *
      *      ENTFERNEN  UNFALL-VERS-KENNZEICHEN UU531S-UNVKZ         *
      *   VERS: 09        VOM  16.05.94       GEÄNDERT  BOHLINGER    *
      *   AFOP0013  EINFÜHRUNG AVBG WIEN                             *
      *      EINFÜHRUNG HAFTUNGSZEITRAUM U. PRPFZIFFER ZUR VBNR, VSNR*
      *      SATZLÄNGE AUF 164 VERGÖSSERT                            *
      *   VERS: 10        VOM  22.09.94       GEÄNDERT  GLEIBS       *
      *   AFOP0032  UU531S-FONDNR EINGEFUEGT                         *
      *   VERS: 2.1       VOM 01.11.06        GEÄNDERT  SCHEIDEL     *
      *             DREISTELLIGER PRODUKTSCHLUESSEL                  *
      *                                                              *
      *                                                              *
      *                                                              *
      ****************************************************************
       01  UU531S-SATZ.
           03 UU531S-KEY.
              05 UU531S-GES             PIC 99.
              05 UU531S-VSNR            PIC 9(15).
*Y2K**        05 UU531S-JH              PIC 99.
              05 UU531S-FAELLIG         PIC 9(6).
011106*       05 UU531S-PRODSCHL        PIC 99.
011106        05 UU531S-PROD-PROD-SCHL  PIC 9(4).
              05 UU531S-PRODART         PIC 9.
              05 UU531S-LFDNR           PIC 99.
           03 UU531S-ERSTELL.
              05 UU531S-ERSTELLDATPA    PIC 9(6).
              05 UU531S-ERSTELLDATOP    PIC 9(6).
              05 UU531S-FILLER          PIC X.
           03 UU531S-VBNR               PIC 9(6).
           03 UU531S-NAME               PIC X(20).
           03 UU531S-ABRECHDAT          PIC 9(6).
           03 UU531S-BUCHDATPART        PIC 9(6).
           03 UU531S-BVRD               PIC XX.
           03 UU531S-PIF.
              05 UU531S-INTVSNR         PIC 9(7).
              05 UU531S-PRZ             PIC 99.
              05 UU531S-PIFREST         PIC X(6).
940516        05 UU531S-PIFREST-R       REDEFINES UU531S-PIFREST.
940516           10 UU531S-PRZ-VBNR     PIC X(1).
940516           10 UU531S-PRZ-VSNR     PIC X(2).
940516           10 UU531S-PRZ-REST     PIC X(3).
           03 UU531S-RESERVE            PIC X(21).
           03 UU531S-AMVERS             REDEFINES UU531S-RESERVE.
              05 UU531S-AMVERS-RESERVE  PIC X(14).
              05 UU531S-AMVERS-DDR-KZ   PIC X.
              05 UU531S-AMVERS-REST     PIC X(6).
           03 UU531S-BAD                REDEFINES UU531S-RESERVE.
              05  UU531S-BAD-LAUFZEIT   PIC 99.
              05  UU531S-BAD-DDRKZ      PIC X.
              05  UU531S-BAD-REST       PIC X(18).
           03 UU531S-BFG                REDEFINES UU531S-RESERVE.
              05 UU531S-BFGKDINH.
               10 UU531S-BFGKDAVERM     PIC 9(8).
              05  UU531S-BFGKDDAT       PIC 9(6).
              05  UU531S-BFGGESPROV     PIC S9(7)V99 PACKED-DECIMAL.
              05  UU531S-BFGREST        PIC XX.
           03 UU531S-ROLAND             REDEFINES UU531S-RESERVE.
              05 UU531S-WERTNR          PIC X.
              05 UU531S-SPARTE          PIC 9.
              05 UU531S-LAUFZEIT.
                 07 UU531S-LAUFZEIT1    PIC 9.
                 07 UU531S-LAUFZEIT2    PIC 9.
              05 UU531S-EINLOESBETR     PIC 9(7)V99.
              05 UU531S-EINLOESKZ       PIC 9.
              05 UU531S-ROLAND-DDR-KZ   PIC X.
              05 UU531S-ROLAND-RESERVE  PIC X(6).
           03 UU531S-GES91              REDEFINES UU531S-RESERVE.
              05  UU531S-GES91-GRUNDPROV PIC 9(02)V999.
              05  UU531S-GES91-REST     PIC X(16).
940516     03 UU531S-AVBG               REDEFINES UU531S-RESERVE.
940516        05  UU531S-HAFTZ-BEGINN   PIC 9(06) PACKED-DECIMAL.
940516        05  UU531S-HAFTZ-ENDE     PIC 9(06) PACKED-DECIMAL.
940516        05  UU531S-INKSTD         PIC 9(06) PACKED-DECIMAL.
940516        05  UU531S-BESTSCHL       PIC X(2).
940516        05  UU531S-AVBG-REST      PIC X(07).
           03 UU531S-FEHLERCODE         PIC 99.
           03 UU531S-PRODEIN.
              05 UU531S-PROVPFLSU       PIC 9(7)V99.
              05 UU531S-PROVSATZ        PIC 99V9.
              05 UU531S-PRORATASU       PIC 9(7)V99.
940516        05 UU531S-JNBEITR         PIC 9(7)V99.
940516     03 UU531S-WA.
              05  UU531S-TARIF          PIC X(04).
940922     03 UU531S-FONDSNR            PIC X(06).
011106*    03 FILLER2                   PIC X(03).
011106     03 FILLER                    PIC X(01).
       01  UU531S-D-SATZ.
           03 UU531S-D-KEY.
              05 UU531S-D-GES           PIC 99.
              05 UU531S-D-VSNR          PIC 9(15).
*Y2K**        05 UU531S-D-JH            PIC 99.
              05 UU531S-D-FAELLIG       PIC 9(6).
011106*       05 UU531S-D-PRODSCHL      PIC 99.
011106        05 UU531S-D-PROD-PROD-SCHL
011106                                  PIC 9(4).
              05 UU531S-D-PRODART       PIC 9.
              05 UU531S-D-LFDNR         PIC 99.
           03 UU531S-D-SATZANZ          PIC 9(7).
           03 UU531S-D-GESPROV-GUT      PIC 9(15)V99.
           03 UU531S-D-GESPROV-BEL      PIC 9(15)V99.
011106*    03 UU531S-D-REST             PIC X(93).
011106     03 UU531S-D-REST             PIC X(91).

