      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0064.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0064.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:15:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0064                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0053  ERMITTELN DES SOG. MELZER-AV'S     * 00040004
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: NAUMANN                                          * 00060000
      *    DATUM:    23.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    31.08.94 10:46:38
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * FEVB0139!23.08.94! 01 ! NEU                       ! NAUMANN  *  00140000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      *                                                                 00149000
      *   FUNKTIONEN:                                                   00150000
      *   PI-VB53-FKT-MELZER-AV      ERMITTELN DES VON-DAT UND DES      00160003
      *                              BIS-DAT DES LETZTEN VB-ARBEITS-    00170003
      *                              VERHÄLTNIS ZU EINER MA-ST-NR.      00170103
      *                              VERTRAGSUMSCHREIBUNGEN UNTERBRECHEN00170203
      *                              SOLCH EINEN ZYKLUS NICHT, DER      00170303
      *                              ERMITTELTE ZEITRAUM LIEGT IMMER IN 00170403
      *                              EINEM EINZIGEN TÄTIGKEITSZEITRAUM. 00170503
      *   EINGABE:                                                      00170603
      *   PI-UPVB0053-MA-ST-NR       MA-ST-NR NUMERISCH                 00170703
      *   AUSGABE:                                                      00170803
      *   PO-UPVB0053-AV-DAT-VON     VON-DAT DES MELZER AV              00170903
      *   PO-UPVB0053-AV-VERTRAGSDAT VERTRAGSBEGINN DES MELZER AV       00171005
      *   PO-UPVB0053-AV-DAT-BIS     BIS-DAT DES MELZER AV              00171103
      *   PO-UPVB0053-RC             0=ES WURDEN GÜLTIGE AV-DATEN GEFUN-00171206
      *                                DEN                              00172004
      *                              1=ES WURDEN KEINE AV-DATEN GEFUNDEN00173004
       01  PI-UPVB0053.                                                 00300000
           05 PI-UPVB0053-FKT                PIC  X(002).               00310000
              88 PI-VB53-FKT-MELZER-AV                    VALUE 'ME'.   00320001
                                                                        00321000
           05 PI-UPVB0053-DATEN.                                        00333000
              10 PI-UPVB0053-MA-ST-NR        PIC S9(009)   COMP.        00340000
              10 FILLER                      PIC  X(040).               00370000
      *                                                                 00400000
       01  PO-UPVB0053.                                                 00410000
           05 PO-UPVB0053-AV-DAT-VON         PIC  X(010).               00420000
           05 PO-UPVB0053-AV-VERTRAGSDAT     PIC  X(010).               00420105
           05 PO-UPVB0053-AV-DAT-BIS         PIC  X(010).               00420203
           05 PO-UPVB0053-RC                 PIC  X(001).               00421006
              88 PO-VB53-FOUND                            VALUE '0'.    00422004
              88 PO-VB53-NOT-FOUND                        VALUE '1'.    00423004
           05 FILLER                         PIC  X(029).               00430003
      *                                                                 00442000
      ******** ENDE COPY-MEMBER (DRVB0064) *******************          00443000
                                                                        00444000
