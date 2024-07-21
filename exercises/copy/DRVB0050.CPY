      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0050.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0050.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:13:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0050                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SATZBESCHREIBUNG DATEI UNFALL/LEBEN-WETTBEWERB 1994        * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: BERGHOFER                                        * 00060000
      *    DATUM:    03.02.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    01.03.94 15:46:27
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0073!03.02.94! 01 ! NEU ERSTELLT              ! BERGHOFER*  00140000
      *         !        !    !                           !          *  00150000
      *         !        !    !                           !          *  00160000
      *         !        !    !                           !          *  00170000
      *                                                              *  00180000
      * ENDE-LOGBUCH                                                 *  00190000
      ****************************************************************  00200000
      *                                                              *  00201000
      * DRVB0050-GES20-NETTO-MIT  NETTOEINHEITEN LEBEN  MIT  VM ANTEIL  00203000
      * DRVB0050-GES20-NETTO-OHNE NETTOEINHEITEN LEBEN  OHNE VM ANTEIL  00204000
      * DRVB0050-GES10-NETTO-MIT  NETTOEINHEITEN UNFALL MIT  VM ANTEIL  00204100
      * DRVB0050-GES10-NETTO-OHNE NETTOEINHEITEN UNFALL OHNE VM ANTEIL  00204200
      *                                                              *  00205000
      ****************************************************************  00210000
       01  DRVB0050-SATZ.                                               00270000
           05 DRVB0050-KEY.                                             00271001
               10 DRVB0050-BETR-NR          PIC 9(7).                   00280003
               10 DRVB0050-MONAT.                                       00281003
                   15 DRVB0050-JJ           PIC 9(2).                   00282003
                   15 DRVB0050-MM           PIC 9(2).                   00283003
           05 DRVB0050-GES20-NETTO-MIT      PIC S9(6)V9 COMP-3.         00290001
           05 DRVB0050-GES20-NETTO-OHNE     PIC S9(6)V9 COMP-3.         00300001
           05 DRVB0050-GES10-NETTO-MIT      PIC S9(6)V9 COMP-3.         00310001
           05 DRVB0050-GES10-NETTO-OHNE     PIC S9(6)V9 COMP-3.         00320001
           05 DRVB0050-UNTST-NR             PIC 9(7).                   00330003
      *                                                                 00450000
      ******** ENDE COPY-MEMBER (DRVB0050) *******************          00460000
                                                                        00470000
