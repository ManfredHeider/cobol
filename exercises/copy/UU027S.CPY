      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU027S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU027S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:47:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER ANTRAGS-HISORIEN-DATEI                     *
      *    ---------------------------------------                     *
      *                                                                *
      *    LAENGE = 556 BYTES        ERSTELLT MAI 84                   *
      *                              RIBBERT DV-D                      *
      *    STAND MAI 1984                                              *
      *                                                                *
      *    GEAENDERT : JANUAR 88                     GARRE             *
      *                ERWEITERUNG DER PROVISIONSSTRUKTUR AUF 20       *
      *                                                                *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU027S.
           10  AHSA                        PIC 99.
           10  AHKEY.
               20  AHMANR                  PIC 9(07)     COMP-3.
               20  AHGES                   PIC 99.
               20  AHVNR                   PIC 9(15)     COMP-3.
      *
      *    BESTANDS-SATZ
      *
           10  AHSATZ.
               20  AHHISTP                 OCCURS 2.
                   30  AHMANRP             PIC 9(7)        COMP-3.
                   30  AHMANRVM            PIC 9(7)        COMP-3.
                   30  AHGDATP             PIC 9(7)        COMP-3.
                   30  AHBETREUER          OCCURS 18.
                       40  AGHMANR         PIC 9(7)        COMP-3.
                       40  AGHPRZ          PIC X.
                       40  AGHSTRSTUFE     PIC 9(3)        COMP-3.
                       40  AGHSTRTEXT      PIC X(4).
                       40  AGHPROZ         PIC 9(4)V9      COMP-3.
               20  AHHISTV.
                   30  AHVNRA              PIC 9(15)       COMP-3.
                   30  AHGDATV             PIC 9(7)        COMP-3.
      *
      *    DUMMY-SATZ
      *
           10  AHDSATZ  REDEFINES  AHSATZ.
               20  AHDANZAHL           PIC S9(7)       COMP-3.
               20  AHDLADEDAT          PIC 9(7)        COMP-3.
               20  AHDELADEDAT         PIC 9(7)        COMP-3.

