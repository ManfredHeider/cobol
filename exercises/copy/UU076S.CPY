      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU076S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU076S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:53:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    01.11.94 14:33:51
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE DIT                          *
      *    ----------------------------------                          *
      *                                                                *
      *    LAENGE = 1150 BYTES       ERSTELLT AM 09.10.84.             *
      *                              BIEHL (FFM.)                      *
      *                                                                *
      *    LAENGE = 1152 BYTES       GEAENDERT   01.11.94              *
      *                              SCHMIDT
      *                                                                *
      *                                                                *
      *     GEAEND. AM 05.05.87: FILLER UM 1 STELLE ERWEITERT;         *
      *                          VBEG/VABL/VAEND MIT VORZEICHEN        *
      *                                                                *
      *     GEAEND. AM 13.10.88: PRODTAB VON 20 AUF OCCURS 50 ERWEITERT*
      *     (BIEHL)              PRODSCHL VON 2 AUF 6 STELLEN ERWEITERT*
      *                          ==> NEUE SATZLAENGE = 1150 (VORH. 590)*
      *                                                                *
      *     GEAEND. AM 17.05.93: IM ZUGE DER PLZ-UMSTELLUNG DAS FELD   *
      *     (BORK )              DIT-POSTPLZ AUF 6 STELLEN ERWEITERT   *
      *                                                                *
      *     GEAEND. AM 14.07.94: COPY SATZ AUF DIE LAENGE DES DATEN-   *
      *                          SATZES ERWEITERT.                     *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU076S.
           10  DIT-VORSATZ.
               20  DIT-V-SA            PIC 99.
               20  DIT-V-DATUM         PIC 9(6).
               20  DIT-V-TEXT          PIC X(10).
               20  FILLER              PIC X(1134).
           10  DIT-SATZ     REDEFINES      DIT-VORSATZ.
               20  DIT-SA              PIC 99.
               20  DIT-GES             PIC 99.
               20  DIT-VNR             PIC 9(15) COMP-3.
               20  DIT-MC              PIC X(17).
               20  DIT-VBNR            PIC 9(7).
               20  DIT-ANR             PIC 9.
               20  DIT-NAME            PIC X(60).
               20  DIT-STR             PIC X(26).
               20  DIT-AUSL            PIC XXX.
               20  DIT-PLZ             PIC X(5).
               20  DIT-ORT             PIC X(21).
               20  DIT-GEBD            PIC S9(7)          COMP-3.
               20  DIT-BERUF           PIC X(4).
               20  DIT-ZAHLW           PIC X.
               20  DIT-VBEG            PIC S9(7)          COMP-3.
               20  DIT-VABL            PIC S9(7)          COMP-3.
               20  DIT-VAEND           PIC S9(7)          COMP-3.
               20  DIT-PRODTAB    OCCURS 50.
                   30 DIT-PRODSCHL     PIC X(6).
                   30 DIT-PRODKTO      PIC S9(6)V999      COMP-3.
                   30 DIT-PRODANL      PIC S9(9)          COMP-3.
               20  DIT-POSTEMPF.
                   30 DIT-POSTNAM      PIC X(36).
                   30 DIT-POSTZUS      PIC X(21).
                   30 DIT-POSTSTR      PIC X(25).
                   30 DIT-POSTPLZ      PIC X(06).
                   30 DIT-POSTORT      PIC X(25).
               20  DIT-REST            PIC X(66).
           10  DIT-NACHSATZ    REDEFINES     DIT-VORSATZ.
               20  DIT-N-SA            PIC 99.
               20  DIT-N-ANZ           PIC 9(7).
               20  FILLER              PIC X(1141).

