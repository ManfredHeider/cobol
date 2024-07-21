      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU034S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU034S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Dec 1998 08:37:26   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:49:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    26.08.94 16:35:26
       01  UU034S.
      ******************************************************************
      ***  SOLL-IST-SATZ  TDS                                        ***
      ***                                                            ***
      ***  GEAENDERT: 26.08.1994  VERSION: 01     WIECHERS           ***
      ***  AUFTRAG:   AFRZ0013 - UMSTELLUNG KUS AUF RACF             ***
      ***                                                            ***
      ******************************************************************
           10  IMODUS                  PIC XX.
*Y2K***    10  IDATUM                  PIC 9(6)        COMP-3.
           10  IDATUM                  PIC 9(7)        COMP-3.
           10  IUHR                    PIC 9(9)        COMP-3.
           10  ITERM                   PIC X(4).
260894*    10  IPERSNR                 PIC 9(7).
260894     10  IUSERID                 PIC X(7).
           10  ITSA                    PIC 99.
           10  ITKEY.
               20  ITMANR               PIC 9(7)        COMP-3.
               20  ITSTATMON            PIC 9(5)        COMP-3.
               20  ITSTATSCHL           PIC 9(2).
      *
      *    SOLL-IST-SATZ.
      *
           10  ITSATZA.
               20  ITEVORGA             PIC S9(7)       COMP-3.
               20  ITSUVORGA            PIC S9(7)       COMP-3.
               20  ITGRUVORGA           PIC S9(7)       COMP-3.
               20  ITEIGEN.
                   30  ITEAEINH         PIC S9(8)V9     COMP-3.
                   30  ITESTCK          PIC S9(7)       COMP-3.
                   30  ITESUM           PIC S9(11)      COMP-3.
                   30  ITEBEITR         PIC S9(9)V99    COMP-3.
               20  ITGRUPPE.
                   30  ITGRAEINH        PIC S9(8)V9     COMP-3.
                   30  ITGRSTCK         PIC S9(7)       COMP-3.
                   30  ITGRSUM          PIC S9(11)      COMP-3.
                   30  ITGRBEITR        PIC S9(9)V99    COMP-3.
               20  ITLEER               PIC X(7).
      ******************************************************************
      *    LAENGE  94  BYTES.
      ******************************************************************
      *

