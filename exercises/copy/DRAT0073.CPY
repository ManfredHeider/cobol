      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0073.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0073.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:48:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: SATZBESCHREIBUNG SCHNITTSTELLENDATEI FÜR      * 00020000
      *                 GRUPPEN-ERMITTLUNG (GW ÜBER RANG)             * 00030000
      *    --------------------------------------------------------   * 00040000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00050000
      ***************************************************************** 00070000
      *    VERSION   01   25.02.1994                                 *  00080003
      ****************************************************************  00090000
      * AENDERUNGEN:                                                 *  00100000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00110000
      * -------------------------------------------------------------*  00120000
      * AFVB0065!22.02.94! 01 ! NEU                       ! SCHÖLLH. *  00130000
      *         !        !    !                           !          *  00140000
      *         !        !    !                           !          *  00150000
      *         !        !    !                           !          *  00160000
      *         !        !    !                           !          *  00170000
      *         !        !    !                           !          *  00180000
      *                                                              *  00190000
      * ENDE-LOGBUCH                                                 *  00200000
      ****************************************************************  00210000
      *                                                                 00220000
      *    SCHNITTSTELLE ZWISCHEN EINEM DATENERMITTLUNGS- UND EINEM     00230000
      *    GRUPPENAUFBEREITUNGS-PGM, DAS EINEN GRUPPENWECHSEL ÜBER      00240000
      *    'RANG' ODER 'ANZAHL' AUSFÜHREN SOLL                          00250000
      *                                                                 00260000
      *    RANGW-KRITERIUM: FALLS RANG-KZ = 'J', ENTHÄLT ES DAS         00310000
      *             RANGWECHSEL-KRITERIM (KANN ZUSAMMENGESETZT SEIN)    00320000
      *    DATEN  : DRUCKAUFBEREITETE DATENZEILE - OHNE RANG-NR         00330000
      *                                                                 00340000
       01  DRAT0073.                                                    00350000
           05  DRAT0073-RANGW-KRITERIUM  PIC  X(030).                   00370000
           05  DRAT0073-DATEN            PIC  X(132).                   00380000
      *                                                                 00390000
      ******** ENDE COPY-MEMBER (DRAT0073) ************                 00400000
      *                                                                 00410002
