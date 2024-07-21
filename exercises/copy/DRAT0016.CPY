      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:36:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010001
      *    COPY-MEMBER: DRUCKAUFBEREITETE FELDER  (DRAT0016)          * 00020001
      *                 - PROGRAMMRAHMEN -                            * 00030001
      *    --------------------------------------------------------   * 00040001
      *    VERSION                                                    * 00050001
      *    --------------------------------------------------------   * 00060001
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00070001
      *    DATUM:    21.01.1993                                       * 00080001
      *    STAND:    05.04.1993                                       * 00090005
      ***************************************************************** 00100001
      *                                                                 00110001
      ***** AKTUELLES DATUM (FORM: TT.MM.JJ)                            00120000
       01  D-AKT-DATUM-KURZ         PIC  X(08).                         00130000
                                                                        00140000
      ***** CICS-DATUM (FORM: TT.MM.JJ)                                 00150000
       01  D-CICS-DATUM-KURZ.                                           00160000
           05  D-CICS-K-TAG         PIC  9(02).                         00170000
           05  FILLER               PIC  X(01) VALUE '.'.               00180000
           05  D-CICS-K-MONAT       PIC  9(02).                         00190000
           05  FILLER               PIC  X(01) VALUE '.'.               00200000
           05  D-CICS-K-JJ          PIC  9(02).                         00210000
                                                                        00220000
      ***** CICS-DATUM (FORM: TT.MM.JJJJ)                               00230000
       01  D-CICS-DATUM-LANG.                                           00240000
           05  D-CICS-L-TAG         PIC  9(02).                         00250000
           05  FILLER               PIC  X(01) VALUE '.'.               00260000
           05  D-CICS-L-MONAT       PIC  9(02).                         00270000
           05  FILLER               PIC  X(01) VALUE '.'.               00280000
           05  D-CICS-L-JAHR        PIC  9(04).                         00290000
                                                                        00300000
      ***** CICS-DATUM (FORM: JJJJ-MM-TT)                               00310000
       01  D-CICS-DATUM-ISO.                                            00320000
           05  D-CICS-I-JAHR        PIC  9(04).                         00330000
           05  FILLER               PIC  X(01) VALUE '-'.               00340000
           05  D-CICS-I-MONAT       PIC  9(02).                         00350000
           05  FILLER               PIC  X(01) VALUE '-'.               00360000
           05  D-CICS-I-TAG         PIC  9(02).                         00370000
                                                                        00380000
      ***** AKTUELLE UHRZEIT, DRUCKAUFBEREITET (FORM: HH:MM:SS)         00390000
       01  D-AKT-ZEIT               PIC  X(08).                         00400000
      ***** AKTUELLE UHRZEIT, DRUCKAUFBEREITET (FORM: HH:MM)            00420000
       01  FILLER REDEFINES D-AKT-ZEIT.                                 00430000
           05  D-AKT-ZEIT-KURZ      PIC  X(05).                         00440000
           05  FILLER               PIC  X(03).                         00450000
                                                                        00451002
      *                                                                 00470001
      ******** ENDE COPY-MEMBER DRUCKAUFBEREITETE FELDER (DRAT0016) *** 00480003
