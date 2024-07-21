      *--------------------------------------------------------------*  00010000
      *    Standard für alle Objekte unter PVCS                         00020000
      *     Datum   :  November 1995                                    00030000
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT                             00040000
      *--------------------------------------------------------------*  00050000
      *                                                                 00060000
      *--------------------------------------------------------------*  00070000
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    00080000
      *--------------------------------------------------------------*  00090000
      *    $Workfile:   DRPA0181.CPY  $                                 00100000
      *    $Revision:   2.0  $                                          00110000
      *                                                                 00120000
      *    $Log:   L:\MF\CPY\DRPA0181.CPv  $                            00130000
      *
      *   Rev 2.0   Feb 27 1999 14:28:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 15:00:02   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 15:04:06   P13PVCS
      *Neuerstellung
      *                                                                 00140000
      *--------------------------------------------------------------*  00150000
      ****************************************************************  00160000
      *    COPY-MEMBER: DRPA0181  Funktion "Verarbstatt. analys."    *  00170000
      *    -------------------------------------------------------   *  00180000
      *    ERSTELLT: Wolff                                           *  00190000
      *    DATUM:    23.01.1997                                      *  00200007
      ****************************************************************  00210000
      *    VERSION   01                                              *  00220000
      ****************************************************************  00230000
      * AENDERUNGEN:                                                 *  00240000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00250000
      * -------------------------------------------------------------*  00260000
      * Daisy 1 !23.01.97! 01 ! Neuerstellung             ! Wolff    *  00270007
      * PEW-Rel.!        !    !                           !          *  00280000
      *         !        !    !                           !          *  00290000
      * PEW-REL.!04.06.98! 02 ! NEU: PEST-VSTAT           ! WOLFF    *  00290000
      * 2.0     !        !    !                           !          *  00290000
      *         !        !    !                           !          *  00300000
      *                                                              *  00310000
      * ENDE-LOGBUCH                                                 *  00320000
      ****************************************************************  00330000
      *                                                              *  00340000
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *  00350000
      *    FÜR DIE FUNKTION DRPA0181                                 *  00360000
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *  00370000
      *            DEM ERR-ERROR-BEREICH UND                         *  00380000
      *            DEM STANDARD-STATUS-BEREICH                       *  00390000
      *            (JEWEILS SEPARATE COPYS)                          *  00400000
      *                                                              *  00410000
      ****************************************************************  00420000
       01 PI-DRPA0181.                                                  00430000
          05 PI-DRPA0181-DATEN.                                         00450000
             10 PI-DRPA0181-VTAB-MAX-USED           PIC  9(002).        00460005
             10 PI-DRPA0181-VSTAT-TABELLE.                              00461003
                15 PI-DRPA0181-VSTAT-TAB            OCCURS 99           00470006
                                       INDEXED BY PI-DRPA0181-VTAB-IND. 00480001
                  20 PI-DRPA0181-VTAB-VSTAT         PIC  X(004).        00490006
                  20 PI-DRPA0181-VTAB-VSTAT-UNR     PIC S9(004) BINARY. 00500006
                  20 PI-DRPA0181-VTAB-ORT           PIC  X(030).        00501008
                  20 PI-DRPA0181-VTAB-BEREICH       PIC  X(010).        00502008
      *                                                                 00510000
       01 PO-DRPA0181.                                                  00520000
          05 PO-DRPA0181-RC                         PIC  X(003).        00530000
             88 PO-DRPA0181-RC-OK                         VALUE 'OK '.  00540000
          05 PO-DRPA0181-DATEN.                                         00550000
             10 PO-DRPA0181-VTAB-ZEILE.                                 00560004
                15 PO-DRPA0181-VSTAT                PIC  X(004).        00561006
                15 PO-DRPA0181-VSTAT-UNR            PIC S9(004) BINARY. 00570005
                15 PO-DRPA0181-ORT                  PIC  X(030).        00570106
                15 PO-DRPA0181-BEREICH              PIC  X(010).        00570206
             10 PO-DRPA0181-FEHLER-ZAHL             PIC S9(004) BINARY. 00571006
040698       10 PO-DRPA0181-PEST-VSTAT              PIC  X(004).        00561006
      *                                                                 00580000
      ******** Ende Copy-Member **************************************  00590000
