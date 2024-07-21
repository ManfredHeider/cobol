      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0067.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0067.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:47:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: SATZBESCHREIBUNG SCHNITTSTELLENDATEI FÜR      * 00020000
      *                 GRUPPEN-ERMITTLUNG (GW ÜBER GW-STEUER)        * 00030002
      *    --------------------------------------------------------   * 00040000
      *    ERSTELLT: NAUMANN                                          * 00050000
      ***************************************************************** 00070000
      *    VERSION   01   25.02.1994                                 *  00080004
      ****************************************************************  00090000
      * AENDERUNGEN:                                                 *  00100000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00110000
      * -------------------------------------------------------------*  00120000
      * AFVB0065!26.01.94! 01 ! NEU                       ! NAUMANN  *  00130000
      *         !        !    !                           !          *  00140000
      *         !        !    !                           !          *  00150000
      *         !        !    !                           !          *  00160000
      *         !        !    !                           !          *  00170000
      *         !        !    !                           !          *  00180000
      *                                                              *  00190000
      * ENDE-LOGBUCH                                                 *  00200000
      ****************************************************************  00210000
      *                                                                 00220000
      *    SCHNITTSTELLE ZWISCHEN EINEM DATENERMITTLUNGS- UND EINEM     00221001
      *    GRUPPENAUFBEREITUNGS-PGM, DAS FÜR DEN GRUPPENWECHSEL DAS     00222002
      *    FELD 'GW-STEUER' AUSWERTET                                   00222102
      *                                                                 00222202
      *    GW-STEUER: ZUORDNUNG DES DATENSATZES ZUR ANGEGEBENEN GRUPPE  00222402
      *    DATEN    : DRUCKAUFBEREITETE DATENZEILE - OHNE LFD-NR        00222601
      *                                                                 00223001
       01  DRAT0067.                                                    00230000
           05  DRAT0067-GW-STEUER        PIC  9(002).                   00240002
           05  DRAT0067-DATEN            PIC  X(132).                   00250000
      *                                                                 00251000
      ******** ENDE COPY-MEMBER (DRAT0067) ************                 00540000
      *                                                                 00550003
