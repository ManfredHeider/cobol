      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0055.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0055.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:14:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0055                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SATZBESCHREIBUNG DATEI UNFALL/LEBEN-WETTBEWERB 1994        * 00040000
      *    (VILLA FLORES)                                             *
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: BERGHOFER                                        * 00060000
      *    DATUM:    11.03.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    25.03.94 11:20:57
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0088!11.03.94! 01 ! NEU ERSTELLT              ! BERGHOFER*  00140000
      *         !        !    !                           !          *  00150000
      *         !        !    !                           !          *  00160000
      *         !        !    !                           !          *  00170000
      *                                                              *  00180000
      * ENDE-LOGBUCH                                                 *  00190000
      ****************************************************************  00200000
      *                                                              *  00201000
      * DRVB0055-VB-NR             MA-ST-NR DES TEILNEHMERS          *  00204000
      * DRVB0055-MONAT             MONAT DES AUSWERTUNGSLAUFS        *
      * DRVB0055-DIR               DIREKTION DES TEILNEHMERS         *  00203000
      * DRVB0055-NAME              NACHNAME DES TEILNEHMERS          *  00204100
      * DRVB0055-VORNAME           VORNAME DES TEILNEHMERS           *  00204200
      * DRVB0055-EINHEITEN         WERTUNGSEINHEITEN DES TEILNEHMERS *  00204200
      * DRVB0055-VILLA-FLORES-KZ   ZEIGT AN,OB BEREITS EIN AUFENTHALT*  00204200
      *                            IN VILLA FLORES GEWONNEN WURDE    *  00205000
      ****************************************************************  00210000
       01  DRVB0055-SATZ.                                               00270000
           05 DRVB0055-KEY.                                             00271001
               10 DRVB0055-VB-NR            PIC 9(007).                 00280003
               10 DRVB0055-MONAT.                                       00281003
                   15 DRVB0055-JJ           PIC 9(002).                 00282003
                   15 DRVB0055-MM           PIC 9(002).                 00283003
           05 DRVB0055-DIR                  PIC 9(003).                 00290001
           05 DRVB0055-NAME                 PIC X(020).                 00300001
           05 DRVB0055-VORNAME              PIC X(018).                 00310001
           05 DRVB0055-EINHEITEN            PIC S9(6)V9 COMP-3.         00320001
           05 DRVB0055-VILLA-FLORES-KZ      PIC X(001).                 00330003
      *                                                                 00450000
      ******** ENDE COPY-MEMBER (DRVB0050) *******************          00460000
                                                                        00470000
