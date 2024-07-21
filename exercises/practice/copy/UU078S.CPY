      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU078S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU078S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:53:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   3    20.07.93 10:01:27
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE   D  M  B                    *
      *    ----------------------------------------------------        *
      *                                                                *
      *    LAENGE = FEST             ERSTELLT   AM 02.10.90            *
      *           = 1138 BYTE                    KUHN                  *
      *                                                                *
      *    LAENGE = FEST             GEAENDERT  AM 07.10.91            *
      *           =  450 BYTE                    KUHN                  *
      *                                                                *
      *    GEÄNDERT 25.05.93 / BORK                                    *
      *    ÄNDERUNG WEGEN DER UMSTELLUNG AUF DIE NEUE, 5-STELLIGE PLZ. *
      *    KEINE ÄNDERUNGEN VORGENOMMEN. PLZ WAR X(005) DEFINIERT.     *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU078S.
         05  DMB-CSATZ.
           10  DMB-SA                  PIC X.
           10  DMB-VNR                 PIC 9(12).
           10  DMB-ORGANR2             PIC 9(8).
           10  DMB-BAKKZ               PIC X.
           10  DMB-VBEG1.
               15  DMB-VBEG-JJ         PIC 99.
               15  DMB-VBEG            PIC 9(6).
           10  DMB-VABL1.
               15  DMB-VABL-JJ         PIC 99.
               15  DMB-VABL            PIC 9(6).
           10  DMB-AEND1.
               15  DMB-AEND-JJ         PIC XX.
               15  DMB-AEND            PIC X(6).
           10  DMB-ZAHLW               PIC 9.
           10  DMB-FAEL                PIC 99.
           10  DMB-LAST                PIC X.
           10  DMB-VSU.
               15  DMB-VSU-T           PIC 9(6).
               15  DMB-VSU-H           PIC 9(3).
           10  DMB-ANREDE              PIC X.
           10  DMB-TITEL               PIC X.
           10  DMB-NAME                PIC X(30).
           10  DMB-AUSL                PIC X(3).
           10  DMB-PLZ.
               20  DMB-PLZ1            PIC X.
               20  DMB-PLZ2            PIC X(4).
           10  DMB-ORT                 PIC X(25).
           10  DMB-STR                 PIC X(30).
           10  DMB-GEBD1.
               15  DMB-GEBD-JJ         PIC XX.
               15  DMB-GEBD            PIC X(6).
           10  DMB-PRAEMIE             PIC 9(8)V9.
           10  FILLER                  PIC X(26).
           10  DMB-RISZAEHL            PIC 9(3).
           10  DMB-VARIABEL OCCURS 05.
               20 DMB-WAG              PIC X(4).
               20 DMB-VTEXT            PIC X(25).
               20 DMB-VART             PIC 9(5).
               20 FILLER               PIC X(16).

         05  DMB-ASATZ  REDEFINES  DMB-CSATZ.
           10  A-FREI-01               PIC X.
           10  DMB-ABS                 PIC X(33).
           10  DMB-ERSTDAT.
               15  DMB-ERST-JJ         PIC 9(2).
               15  DMB-ERST            PIC 9(6).
           10  DMB-EMPF                PIC X(31).
           10  DMB-INHALT              PIC X(31).
           10  A-FREI-02               PIC X(346).

         05  DMB-ESATZ  REDEFINES  DMB-CSATZ.
           10  C-FREI-01               PIC X.
           10  DMB-SATZANZ             PIC 9(7).
           10  C-FREI-02               PIC X(442).

