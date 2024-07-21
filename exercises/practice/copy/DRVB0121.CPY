      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0121.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0121.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 17 1996 15:27:02   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0121, Zuschlagstabelle für Zielwettbewerb *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MOLL                                             *
      *    DATUM:    11.12.1996                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * AFVB0329!11.12.96!    ! NEU                       ! Moll      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *   BESCHREIBUNG DER TABELLE:
      *
      *   Prozentualer Zuschlag für Zielermittlung in Abhängigkeit
      *   des erreichten monatlichen Durechschnitts.
      *
      ***************************************************************
      * -------------------------------------------------------------
      * mtl. EH <-> proz. Zuschlag
      * -------------------------------------------------------------
       01 ZK-ZUSCHLAG-VALUES.
          05 ZK-ZUSCHLAG-TAB-VALUE.
      *                  ** Zielvorgaben **
             10 ZK-ZUSCHLAG.
                15                PIC  S9(09)V9  COMP-3 VALUE      0.
                15                PIC  S9(03)V99 COMP-3 VALUE  100,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    100.
                15                PIC  S9(03)V99 COMP-3 VALUE  100,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    200.
                15                PIC  S9(03)V99 COMP-3 VALUE   98,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    300.
                15                PIC  S9(03)V99 COMP-3 VALUE   96,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    400.
                15                PIC  S9(03)V99 COMP-3 VALUE   94,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    500.
                15                PIC  S9(03)V99 COMP-3 VALUE   92,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    600.
                15                PIC  S9(03)V99 COMP-3 VALUE   90,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    700.
                15                PIC  S9(03)V99 COMP-3 VALUE   88,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    800.
                15                PIC  S9(03)V99 COMP-3 VALUE   86,0.

                15                PIC  S9(09)V9  COMP-3 VALUE    900.
                15                PIC  S9(03)V99 COMP-3 VALUE   84,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1000.
                15                PIC  S9(03)V99 COMP-3 VALUE   82,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1100.
                15                PIC  S9(03)V99 COMP-3 VALUE   80,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1200.
                15                PIC  S9(03)V99 COMP-3 VALUE   78,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1300.
                15                PIC  S9(03)V99 COMP-3 VALUE   76,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1400.
                15                PIC  S9(03)V99 COMP-3 VALUE   74,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1500.
                15                PIC  S9(03)V99 COMP-3 VALUE   72,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1600.
                15                PIC  S9(03)V99 COMP-3 VALUE   70,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1700.
                15                PIC  S9(03)V99 COMP-3 VALUE   68,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1800.
                15                PIC  S9(03)V99 COMP-3 VALUE   66,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   1900.
                15                PIC  S9(03)V99 COMP-3 VALUE   64,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2000.
                15                PIC  S9(03)V99 COMP-3 VALUE   62,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2100.
                15                PIC  S9(03)V99 COMP-3 VALUE   60,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2200.
                15                PIC  S9(03)V99 COMP-3 VALUE   59,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2300.
                15                PIC  S9(03)V99 COMP-3 VALUE   58,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2400.
                15                PIC  S9(03)V99 COMP-3 VALUE   57,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2500.
                15                PIC  S9(03)V99 COMP-3 VALUE   56,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2600.
                15                PIC  S9(03)V99 COMP-3 VALUE   55,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2700.
                15                PIC  S9(03)V99 COMP-3 VALUE   54,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2800.
                15                PIC  S9(03)V99 COMP-3 VALUE   53,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   2900.
                15                PIC  S9(03)V99 COMP-3 VALUE   52,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   3000.
                15                PIC  S9(03)V99 COMP-3 VALUE   51,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   3100.
                15                PIC  S9(03)V99 COMP-3 VALUE   50,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   3200.
                15                PIC  S9(03)V99 COMP-3 VALUE   49,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   3300.
                15                PIC  S9(03)V99 COMP-3 VALUE   48,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   3400.
                15                PIC  S9(03)V99 COMP-3 VALUE   47,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   3500.
                15                PIC  S9(03)V99 COMP-3 VALUE   46,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   3600.
                15                PIC  S9(03)V99 COMP-3 VALUE   45,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   3700.
                15                PIC  S9(03)V99 COMP-3 VALUE   44,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   3800.
                15                PIC  S9(03)V99 COMP-3 VALUE   44,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   3900.
                15                PIC  S9(03)V99 COMP-3 VALUE   43,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   4000.
                15                PIC  S9(03)V99 COMP-3 VALUE   42,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   4100.
                15                PIC  S9(03)V99 COMP-3 VALUE   41,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   4200.
                15                PIC  S9(03)V99 COMP-3 VALUE   40,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   4300.
                15                PIC  S9(03)V99 COMP-3 VALUE   40,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   4400.
                15                PIC  S9(03)V99 COMP-3 VALUE   39,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   4500.
                15                PIC  S9(03)V99 COMP-3 VALUE   38,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   4600.
                15                PIC  S9(03)V99 COMP-3 VALUE   37,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   4700.
                15                PIC  S9(03)V99 COMP-3 VALUE   36,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   4800.
                15                PIC  S9(03)V99 COMP-3 VALUE   36,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   4900.
                15                PIC  S9(03)V99 COMP-3 VALUE   35,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   5000.
                15                PIC  S9(03)V99 COMP-3 VALUE   34,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   5100.
                15                PIC  S9(03)V99 COMP-3 VALUE   33,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   5200.
                15                PIC  S9(03)V99 COMP-3 VALUE   33,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   5300.
                15                PIC  S9(03)V99 COMP-3 VALUE   32,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   5400.
                15                PIC  S9(03)V99 COMP-3 VALUE   32,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   5500.
                15                PIC  S9(03)V99 COMP-3 VALUE   31,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   5600.
                15                PIC  S9(03)V99 COMP-3 VALUE   30,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   5700.
                15                PIC  S9(03)V99 COMP-3 VALUE   30,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   5800.
                15                PIC  S9(03)V99 COMP-3 VALUE   29,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   5900.
                15                PIC  S9(03)V99 COMP-3 VALUE   29,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   6000.
                15                PIC  S9(03)V99 COMP-3 VALUE   28,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   6100.
                15                PIC  S9(03)V99 COMP-3 VALUE   27,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   6200.
                15                PIC  S9(03)V99 COMP-3 VALUE   27,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   6300.
                15                PIC  S9(03)V99 COMP-3 VALUE   26,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   6400.
                15                PIC  S9(03)V99 COMP-3 VALUE   26,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   6500.
                15                PIC  S9(03)V99 COMP-3 VALUE   25,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   6600.
                15                PIC  S9(03)V99 COMP-3 VALUE   24,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   6700.
                15                PIC  S9(03)V99 COMP-3 VALUE   24,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   6800.
                15                PIC  S9(03)V99 COMP-3 VALUE   23,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   6900.
                15                PIC  S9(03)V99 COMP-3 VALUE   23,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   7000.
                15                PIC  S9(03)V99 COMP-3 VALUE   22,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   7100.
                15                PIC  S9(03)V99 COMP-3 VALUE   21,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   7200.
                15                PIC  S9(03)V99 COMP-3 VALUE   21,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   7300.
                15                PIC  S9(03)V99 COMP-3 VALUE   20,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   7400.
                15                PIC  S9(03)V99 COMP-3 VALUE   20,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   7500.
                15                PIC  S9(03)V99 COMP-3 VALUE   20,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   7600.
                15                PIC  S9(03)V99 COMP-3 VALUE   19,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   7700.
                15                PIC  S9(03)V99 COMP-3 VALUE   19,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   7800.
                15                PIC  S9(03)V99 COMP-3 VALUE   18,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   7900.
                15                PIC  S9(03)V99 COMP-3 VALUE   18,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   8000.
                15                PIC  S9(03)V99 COMP-3 VALUE   18,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   8100.
                15                PIC  S9(03)V99 COMP-3 VALUE   17,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   8200.
                15                PIC  S9(03)V99 COMP-3 VALUE   17,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   8300.
                15                PIC  S9(03)V99 COMP-3 VALUE   16,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   8400.
                15                PIC  S9(03)V99 COMP-3 VALUE   16,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   8500.
                15                PIC  S9(03)V99 COMP-3 VALUE   16,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   8600.
                15                PIC  S9(03)V99 COMP-3 VALUE   15,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   8700.
                15                PIC  S9(03)V99 COMP-3 VALUE   15,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   8800.
                15                PIC  S9(03)V99 COMP-3 VALUE   14,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   8900.
                15                PIC  S9(03)V99 COMP-3 VALUE   14,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   9000.
                15                PIC  S9(03)V99 COMP-3 VALUE   14,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   9100.
                15                PIC  S9(03)V99 COMP-3 VALUE   13,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   9200.
                15                PIC  S9(03)V99 COMP-3 VALUE   13,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   9300.
                15                PIC  S9(03)V99 COMP-3 VALUE   12,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   9400.
                15                PIC  S9(03)V99 COMP-3 VALUE   12,4.

                15                PIC  S9(09)V9  COMP-3 VALUE   9500.
                15                PIC  S9(03)V99 COMP-3 VALUE   12,0.

                15                PIC  S9(09)V9  COMP-3 VALUE   9600.
                15                PIC  S9(03)V99 COMP-3 VALUE   11,6.

                15                PIC  S9(09)V9  COMP-3 VALUE   9700.
                15                PIC  S9(03)V99 COMP-3 VALUE   11,2.

                15                PIC  S9(09)V9  COMP-3 VALUE   9800.
                15                PIC  S9(03)V99 COMP-3 VALUE   10,8.

                15                PIC  S9(09)V9  COMP-3 VALUE   9900.
                15                PIC  S9(03)V99 COMP-3 VALUE   10,4.

                15                PIC  S9(09)V9  COMP-3 VALUE  10000.
                15                PIC  S9(03)V99 COMP-3 VALUE   10,0.

       01 ZK-ZS-TABELLE REDEFINES ZK-ZUSCHLAG-VALUES.
      *   *  OCCURS auch als Konstante K-ZK-ZS-OCCURS definiert!
          05 ZK-ZS-TAB OCCURS 101
                       INDEXED BY ZK-ZS-TAB-IND.
             10 ZK-ZS.
                15 ZK-ZS-EH       PIC  S9(09)V9  COMP-3.
                15 ZK-ZS-PROZ     PIC  S9(03)V99 COMP-3.

       01 K-ZK-ZS-OCCURS          PIC  S9(04)    BINARY VALUE 101.
      *                                                      ^^^^^

      ******** ENDE COPY-MEMBER *************************************
