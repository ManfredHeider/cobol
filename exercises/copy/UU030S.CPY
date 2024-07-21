      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU030S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/UU030S.CPv  $
      *
      *   Rev 2.1   Jun 23 2003 12:12:36   t13ae34
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   17 Dec 1998 08:36:18   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.1   03 Sep 1998 13:09:40   T13EX36
      *AAN00164 - Jahr2000 Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:48:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
       01  UU030S.
      ******************************************************************
      ***  SOLL-IST-SATZ                                             ***
      ******************************************************************
           10  ISA                     PIC 99.
           10  IKEY.
               20  IMANR               PIC 9(7)        COMP-3.
*Y2K***        20  ISTATMON            PIC 9(4)        COMP-3.
               20  ISTATMON            PIC 9(5)        COMP-3.
               20  ISTATSCHL           PIC 9(2).
      *
      *    SOLL-IST-SATZ.
      *
           10  ISATZA.
               20  IEVORGA             PIC S9(7)       COMP-3.
               20  ISUVORGA            PIC S9(7)       COMP-3.
               20  IGRUVORGA           PIC S9(7)       COMP-3.
               20  IEIGEN.
                   30  IEAEINH         PIC S9(8)V9     COMP-3.
                   30  IESTCK          PIC S9(7)       COMP-3.
                   30  IESUM           PIC S9(11)      COMP-3.
                   30  IEBEITR         PIC S9(9)V99    COMP-3.
               20  IGRUPPE.
                   30  IGRAEINH        PIC S9(8)V9     COMP-3.
                   30  IGRSTCK         PIC S9(7)       COMP-3.
                   30  IGRSUM          PIC S9(11)      COMP-3.
                   30  IGRBEITR        PIC S9(9)V99    COMP-3.
               20  ILEER               PIC X(7).
      *
      *    VORSATZ.
      *
           10  ISATZB REDEFINES ISATZA.
               20  IANZAHL             PIC 9(7)        COMP-3.
               20  ILADEDAT            PIC 9(6)        COMP-3.
               20  IENTLADEDAT         PIC 9(6)        COMP-3.
               20  IVON                PIC 9(6)        COMP-3.
               20  IBIS                PIC 9(6)        COMP-3.
               20  IFILLER             PIC X(41).
      ******************************************************************
      *    LAENGE  72  BYTES.
      ******************************************************************
      *
      *

