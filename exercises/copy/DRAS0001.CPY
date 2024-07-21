      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAS0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   01 Sep 1998 14:02:54   T13EX35
      *AVB00426 - Anpassung Jahr2000
      *
      *   Rev 1.0   30 Jul 1998 09:30:14   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      **************************************************************
      *                                                            *
      * COPY STRUKTUR ALS EXTENDED VERSION ALS ERSATZ FUER DIE     *
      * IM PROGRAMM UU171 FEST DEFINIERTEN ANGABEN FUER DEN        *
      * EFILE2 UND DEN AFILE1                                      *
      * COPY MEMBER FUER DIE VSAM-DATEI UU004100.I1.CL EXTENDED    *
      *                                                            *
      **************************************************************

       01  UU041S.
           05   EA-KEY.
             10 EA-DIR                     PIC 999.
             10 EA-JH                      PIC 99.
             10 EA-JJMM                    PIC 9(4).
           05   EA-REGBEZ                  PIC X(20).
           05   EA-VM-ANZ                  PIC S9(9)   COMP.
           05   EA-GES-EH                  PIC S9(9)V9 COMP-3.
           05   EA-VM-EH                   PIC S9(9)V9 COMP-3.
           05   EA-QUOTE                PIC S9(9)V9 COMP-3.

