      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU068S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU068S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:52:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    27.09.93 17:14:28
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE III
      *    ----------------------------------                          *
      *                                                                *
      *    LAENGE = 589 BYTES        ERSTELLT AM 12.04.84.             *
      *                              BIEHL (FFM.)                      *
      *                                                                *
      *    GEÄNDERT 25.05.93 / BORK                                    *
      *    ÄNDERUNG WEGEN UMSTELLUNG AUF DIE NEUE, 5-STELLIGE PLZ.     *
      *    IM HAUPTTEIL ERFOLGT KEINE ÄNDERUNG, IM TEIL FÜR DEN POST-  *
      *    EMPFÄNGER WURDE DIE PLZ AUF 5 STELLEN ERWEITERT.            *
      *                                                                *
      *    GEÄNDERT 23.09.93 / BORK                                    *
      *    ÄNDERUNG WEGEN NEUEM SATZAUFBAU VON III. ALLE DATUMSFELDER  *
      *    KOMMEN JETZT IM FORMAT TTMMJJ. DIE GEPACKTEN DATUMS-FELDER *
      *    WURDEN ALLE MIT VORZEICHEN VERSEHEN.                        *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU068S.
           10  III-VORSATZ.
               20  III-V-SA            PIC 99.
               20  III-V-DATUM         PIC 9(6).
               20  III-V-TEXT          PIC X(10).
               20  FILLER              PIC X(571).
           10  III-SATZ     REDEFINES      III-VORSATZ.
               20  III-SA              PIC 99.
               20  III-GES             PIC 99.
               20  III-VNR             PIC 9(15) COMP-3.
               20  III-MC              PIC X(17).
               20  III-VBNR            PIC 9(7).
               20  III-ANR             PIC 9.
               20  III-NAME            PIC X(60).
               20  III-STR             PIC X(26).
               20  III-AUSL            PIC XXX.
               20  III-PLZ             PIC X(5).
               20  III-ORT             PIC X(21).
230993         20  III-GEBD            PIC S9(7)         COMP-3.
               20  III-BERUF           PIC X(4).
               20  III-ZAHLW           PIC X.
230993         20  III-VBEG            PIC S9(7)         COMP-3.
230993         20  III-VABL            PIC S9(7)         COMP-3.
230993         20  III-VAEND           PIC S9(7)         COMP-3.
               20  III-PRODTAB    OCCURS 20.
                   30 III-PRODSCHL     PIC XX.
                   30 III-PRODKTO      PIC 9(6)V999       COMP-3.
                   30 III-PRODANL      PIC 9(9)           COMP-3.
               20  III-POSTEMPF.
                   30 III-POSTNAM      PIC X(36).
                   30 III-POSTZUS      PIC X(21).
                   30 III-POSTSTR      PIC X(25).
250593             30 III-POSTPLZ      PIC X(05).
                   30 III-POSTORT      PIC X(25).
250593         20  III-REST            PIC X(64).
           10  III-NACHSATZ    REDEFINES     III-VORSATZ.
               20  III-N-SA            PIC 99.
               20  III-N-ANZ           PIC 9(7).
               20  FILLER              PIC X(580).

