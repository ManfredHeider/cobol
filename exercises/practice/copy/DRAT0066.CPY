      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0066.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0066.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:47:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: SATZBESCHREIBUNG SCHNITTSTELLENDATEI FÜR      * 00020000
      *                 DRUCK-PGM'S VON RANGLISTEN                    * 00030000
      *    --------------------------------------------------------   * 00040000
      *    ERSTELLT: NAUMANN                                          * 00050000
      ***************************************************************** 00070000
      *    VERSION   01   25.02.1994                                 *  00080006
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
       01  DRAT0066.                                                    00230000
           05  DRAT0066-SATZART          PIC  X(002).                   00240000
           05  FILLER                    PIC  X(136).                   00250004
                                                                        00260001
      *    SATZBESCHREIBUNG FÜR SATZART  = 'SA'                         00270000
      *     (STEUERINFORMATIONEN ZU NEUER LISTE)                        00280001
       01  DRAT0066-SA REDEFINES DRAT0066.                              00290000
           05  FILLER                    PIC  X(002).                   00300000
           05  DRAT0066-SA-LIST-ID       PIC  9(004).                   00310004
           05  FILLER                    PIC  X(132).                   00320000
                                                                        00330001
      *    SATZBESCHREIBUNG FÜR SATZART  = 'SB'                         00340000
      *     (STEUERINFORMATIONEN ZU NEUER GRUPPE)                       00350001
       01  DRAT0066-SB REDEFINES DRAT0066.                              00360000
           05  FILLER                    PIC  X(002).                   00370000
           05  DRAT0066-SB-LIST-ID       PIC  9(004).                   00380004
           05  DRAT0066-SB-GRP-ID        PIC  9(004).                   00390004
           05  FILLER                    PIC  X(128).                   00400004
                                                                        00410001
      *    SATZBESCHREIBUNG FÜR SATZART  = 'DA'                         00420000
      *     (DATEN: EINFACHE DATENZEILE)                                00430001
       01  DRAT0066-DA REDEFINES DRAT0066.                              00440000
           05  FILLER                    PIC  X(002).                   00450000
           05  DRAT0066-DA-BLOCK-LFD     PIC  9(004).                   00460004
           05  DRAT0066-DA-DATEN         PIC  X(132).                   00470000
                                                                        00480001
      *    SATZBESCHREIBUNG FÜR SATZART  = 'DB'                         00490000
      *     (DATEN: GRUPPEN-KOPF-ZEILE)                                 00500001
       01  DRAT0066-DB REDEFINES DRAT0066.                              00510000
           05  FILLER                    PIC  X(002).                   00520000
           05  DRAT0066-DB-FLEERZ        PIC  9(004).                   00521004
           05  DRAT0066-DB-DATEN         PIC  X(132).                   00530001
                                                                        00540001
      *    SATZBESCHREIBUNG FÜR SATZART  = 'DC'                         00550000
      *     (DATEN: GRUPPEN-FUSS-ZEILE)                                 00560001
       01  DRAT0066-DC REDEFINES DRAT0066.                              00570000
           05  FILLER                    PIC  X(002).                   00580000
           05  DRAT0066-DC-FLEERZ        PIC  9(004).                   00581004
           05  DRAT0066-DC-DATEN         PIC  X(132).                   00590001
                                                                        00591005
      *    SATZBESCHREIBUNG FÜR SATZART  = 'DE'                         00592005
      *     (DATEN: SPALTEN-KOPF-ZEILE)                                 00593005
       01  DRAT0066-DE REDEFINES DRAT0066.                              00594005
           05  FILLER                    PIC  X(002).                   00595005
           05  DRAT0066-DE-FLEERZ        PIC  9(004).                   00596005
           05  DRAT0066-DE-DATEN         PIC  X(132).                   00597005
      *                                                                 00600000
      ******** ENDE COPY-MEMBER (DRAT0066) ************                 00610000
                                                                        00620001
