      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU031S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/UU031S.CPv  $
      *
      *   Rev 2.2   28 Mar 2007 15:57:08   t13ae23
      *AAN00655: Batchabläufe Täglich - Erweiterung des PS auf 3 St
      *ellen 
      *
      *   Rev 2.1   Oct 13 1999 16:39:48   T13AE34
      *FAN00227-Korr. Proderg
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   17 Dec 1998 08:36:50   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.1   03 Sep 1998 13:10:10   T13EX36
      *AAN00164 - Jahr2000 Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:48:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ****************************************************************
      *    COPY-MEMBER: UU031S                                       *
      *    FUNKTION   : SATZBESCHREIBUNG PRODUKTIONSERGEBNISDATEI    *
      *                 UU001000 CICS DD0010)                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    13.10.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN00227!13.10.99!2.1 !BEITRAGSFELDER VERGRÖSSERN !HEIDER    *
      * AAN00655!28.03.07!2.2 !DREISTELLIGE PRODUKTSCHL.  !RAHNER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      *******     PRODUKTIONSERGEBNISSATZ                  *************
      ******************************************************************
CCC009*    SKIP2
       01  UU031S.
           10  PSA                     PIC 99.
           10  PKEY.
               20  PMANR               PIC 9(7)        COMP-3.
*Y2K***        20  PSTATDAT            PIC 9(6)        COMP-3.
               20  PSTATDAT            PIC 9(7)        COMP-3.
280307*        20  PSTATSCHL           PIC 9(5)        COMP-3.
280307         20  PSTATSCHL.
280307             30  PSTATSCHL-ERG-KZ          PIC 9(1).
280307             30  PSTATSCHL-PGEB-GES        PIC 9(003) COMP-3.
280307             30  PSTATSCHL-PROD-PROD-SCHL  PIC 9(003) COMP-3.
      *
      *    PRODUKTIONS-ERGEBNIS-SATZ.
           10  PSATZA.
               20  PEIGEN.
                   30  PEAEINH             PIC S9(8)V9     COMP-3.
                   30  PESTCK              PIC S9(7)       COMP-3.
                   30  PESUM               PIC S9(11)      COMP-3.
131099*            30  PEBEITR             PIC S9(9)V99    COMP-3.
131099             30  PEBEITR             PIC S9(11)V99    COMP-3.
               20  PGRUPPE.
                   30  PGRAEINH            PIC S9(8)V9     COMP-3.
                   30  PGRSTCK             PIC S9(7)       COMP-3.
                   30  PGRSUM              PIC S9(11)      COMP-3.
131099*            30  PGRBEITR            PIC S9(9)V99    COMP-3.
131099             30  PGRBEITR            PIC S9(11)V99   COMP-3.
131099*        20  PLEER                   PIC X(9).
280307*        20  PLEER                   PIC X(7).
280307         20  PLEER                   PIC X(5).
      *
      *    VORSATZ.
      *
           10  PSATZB REDEFINES PSATZA.
               20  PANZAHL             PIC S9(7)       COMP-3.
               20  PLADEDAT            PIC 9(6)        COMP-3.
               20  PENTLADEDAT         PIC 9(6)        COMP-3.
               20  PVONMON             PIC 9(6)        COMP-3.
               20  PVONTAG             PIC 9(6)        COMP-3.
               20  PBIS                PIC 9(6)        COMP-3.
280307*        20  PFILLER             PIC X(27).
280307         20  PFILLER             PIC X(25).
      *
CCC009*    SKIP2
      ******************************************************************
      *    *** LAENGE 64 BYTES ***
      ******************************************************************
      *

